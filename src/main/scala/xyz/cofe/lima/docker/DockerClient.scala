package xyz.cofe.lima.docker

import xyz.cofe.lima.docker.http.{Decoder, HttpLogger, HttpRequest, HttpResponse, HttpResponseReader, HttpResponseStream, SocketChannelSupplier, SocketLogger}

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.HttpResponseStream.Event
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.docker.log.Logger.{defaultLogger, _}
import xyz.cofe.lima.docker.model.{ContainerFileChanges, CreateContainerRequest, CreateContainerResponse, Image, ImageHistory, ImageInspect, ImagePullStatusEntry, ImageRemove}

import java.net.UnixDomainSocketAddress
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.{Lock, ReentrantLock}

case class DockerClient( socketChannel: SocketChannel,
                         openSocket: ()=>SocketChannel,
                         sourceTimeout:Long=1000L,
                         readTimeout:  Long=1000L*30L,
                         cpuThrottling:Long=1,
                         streamReadTimeout:Long=(-1),
                         streamSourceTimeout:Long=1000L*30L,
                         socketLock: Lock = new ReentrantLock(),
                         clientId:Int = DockerClient.idSeq.incrementAndGet(),
                       )
                       (implicit
                        httpLogger: HttpLogger,
                        socketLogger: SocketLogger,
                        logger: Logger
                       )
{
  def newClient:DockerClient =
    DockerClient(
      openSocket(),
      openSocket,
      sourceTimeout,
      readTimeout,
      cpuThrottling,
      streamReadTimeout,
      streamSourceTimeout,
      socketLock = new ReentrantLock(),
      clientId = DockerClient.idSeq.incrementAndGet()
    )(httpLogger,socketLogger,logger)

  def withLogger(httpLogger: HttpLogger):DockerClient =
    DockerClient(
      socketChannel,
      openSocket,
      sourceTimeout, readTimeout, cpuThrottling, streamReadTimeout, streamSourceTimeout,
      socketLock
    )(httpLogger,socketLogger,logger)

  def withLogger(logger: Logger):DockerClient =
    DockerClient(
      socketChannel,
      openSocket,
      sourceTimeout, readTimeout, cpuThrottling, streamReadTimeout, streamSourceTimeout,
      socketLock
    )(httpLogger,socketLogger,logger)

  def lockAndRun[R]( code: => R ):R = {
    try {
      socketLock.lock()
      code
    }finally {
      socketLock.unlock()
    }
  }
  def tryLockAndRun[R]( code: =>R ):Option[R] = {
    var succLock = false
    try {
      if( socketLock.tryLock() ) {
        succLock = true
        Some(code)
      }else{
        None
      }
    }finally {
      if( succLock ) {
        socketLock.unlock()
      }
    }
  }

  implicit val clientIdProvide: ClientId = new Logger.ClientId {
    override def clientId: Int = DockerClient.this.clientId
  }

  //#region http tasks

  socketChannel.configureBlocking(false)

  private def sendRequest(request:HttpRequest):Unit = {
    lockAndRun {
      val headerBlock =
        ((request.method + " " + request.path + " " + request.proto + "\n") +
          ("HOST: " + request.host + "\n") +
          ("User-Agent: " + request.`User-Agent` + "\n") +
          ("Accept: " + request.Accept + "\n") +
          (request.otherHeaders.map { case (k, v) => k + ": " + v }.mkString("\n")) +
          "\n" + (if(request.body.nonEmpty) "\n" else "" )
          ).getBytes(StandardCharsets.UTF_8)

      val sendBB = ByteBuffer.allocate(headerBlock.size + request.body.size)
      sendBB.put(headerBlock)

      val bodyArr = request.body.toArray
      sendBB.put(bodyArr)
      sendBB.flip()

      socketLogger.send(sendBB)
      socketChannel.write(sendBB)
    }
  }

  def stream(request:HttpRequest)(consumer:HttpResponseStream.Event=>HttpResponseStream.Behavior):Unit = {
    lockAndRun {
      httpLogger.send(request)
      sendRequest(request)
      HttpResponseStream(
        SocketChannelSupplier(socketChannel),
        sourceTimeout = streamSourceTimeout,
        readTimeout = streamReadTimeout,
        cpuThrottling = cpuThrottling,
        pid = request.id
      ).read(consumer)
    }
  }

  def sendForHttp(request:HttpRequest):Either[String,HttpResponse] = {
    lockAndRun {
      httpLogger.send(request)
      sendRequest(request)
      val socketChannelSupplier = SocketChannelSupplier(socketChannel)
      HttpResponseReader(
        socketChannelSupplier,
        sourceTimeout = sourceTimeout,
        readTimeout = readTimeout,
        cpuThrottling = cpuThrottling,
        pid = request.id
      ).read.left.map(err => {
        httpLogger.error(err)
        err
      }).map(resp => {
        httpLogger.receive(resp)
        resp
      })
    }
  }

  def sendForText(
    request:HttpRequest,
    responseWrapper:HttpResponse=>HttpResponse = r=>r,
    successHttpCode:HttpResponse=>Boolean = r=>r.isOk
  ):Either[String,String] = {
    lockAndRun {
      for {
        _ <- Right(httpLogger.send(request))
        response0 <- sendForHttp(request)
        response = responseWrapper(response0)
        _ <- Right(httpLogger.receive(response))
        _ <- if (successHttpCode(response)) {
          Right(response)
        } else {
          Left(s"response not ok\ncode = ${response.code}\ntext = ${response.text}")
        }
        text <- response.text.map(
          s => Right(s)
        ).getOrElse(
          Left("text response not available")
        )
      } yield text
    }
  }

  def sendForJson[A:JsonReader](
                          request:HttpRequest,
                          responseWrapper:HttpResponse=>HttpResponse = r=>r,
                          successHttpCode:HttpResponse=>Boolean = r=>r.isOk
                        ):Either[String,A] = {
    lockAndRun {
      for {
        response0 <- sendForHttp(request)
        response = responseWrapper(response0)
        _ <- if (successHttpCode(response)) {
          Right(response)
        } else {
          Left(s"response not ok\ncode = ${response.code}\ntext = ${response.text}")
        }
        text <- response.text.map(
          s => Right(s)
        ).getOrElse(
          Left("text response not available")
        )
        res <- text.jsonAs[A].left.map(err => err.getMessage)
      } yield res
    }
  }
  //#endregion
  //#region containers tasks

  /**
   * Получение списка контейнеров
   * @param all запросить все контейнеры
   * @param limit ограничить размер выборки
   * @param size получить информацию о размере контейнера
   * @return контейнеры
   */
  def containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false)
  : Either[String, List[model.ContainerStatus]] = {
    logger(Containers(all,limit,size)).run {
      val q = Map("all" -> all.toString) ++
        (limit.map(l => Map("limit" -> l.toString)).getOrElse(Map())) ++
        (if (size) Map("size" -> size.toString) else (Map()))

      sendForJson[List[model.ContainerStatus]](
        HttpRequest(path = "/containers/json").queryString(q)
      )
    }
  }

  /**
   * Получение информации о контейнере
   * @param id идентификатор контейнера
   * @return инфа
   */
  def containerInspect(id:String): Either[String, model.ContainerInspect] =
    logger(ContainerInspect(id)).run {
      sendForJson[model.ContainerInspect](
        HttpRequest(path = s"/containers/${id}/json")
      )
    }

  /**
   * Получение информации о процессах внутри контейнера
   * @param id идентификатор контейнера
   * @return процессы
   */
  def containerProcesses(id:String): Either[String, model.Top] =
    logger(ContainerProcesses(id)).run {
      sendForJson[model.Top](
        HttpRequest(path = s"/containers/${id}/top")
      )
    }

  /**
   * Получение логов контейнера
   *
   * https://docs.docker.com/engine/api/v1.41/#tag/Container/operation/ContainerAttach
   *
   * Stream format
   * When the TTY setting is disabled in POST /containers/create, the stream over the hijacked connected is multiplexed to separate out stdout and stderr. The stream consists of a series of frames, each containing a header and a payload.
   *
   * The header contains the information which the stream writes (stdout or stderr). It also contains the size of the associated frame encoded in the last four bytes (uint32).
   *
   * It is encoded on the first eight bytes like this:
   *
   * header := [8]byte{STREAM_TYPE, 0, 0, 0, SIZE1, SIZE2, SIZE3, SIZE4}
   *
   * STREAM_TYPE can be:
   *
   * 0: stdin (is written on stdout)
   * 1: stdout
   * 2: stderr
   *
   * SIZE1, SIZE2, SIZE3, SIZE4 are the four bytes of the uint32 size encoded as big endian.
   *
   * Following the header is the payload, which is the specified number of bytes of STREAM_TYPE.
   *
   * The simplest way to implement this protocol is the following:
   *
   * Read 8 bytes.
   * Choose stdout or stderr depending on the first byte.
   * Extract the frame size from the last four bytes.
   * Read the extracted size and output it on the correct output.
   * Goto 1.
   * @param id идентификатор контейнера
   * @param follow
   * @param stdout
   * @param stderr
   * @param since
   * @param timestamps
   * @param tail
   * @return
   */
  def containerLogs(id:String,
                    follow:Option[Boolean]=None,
                    stdout:Option[Boolean]=Some(true),
                    stderr:Option[Boolean]=None,
                    since:Option[Long]=None,
                    timestamps:Option[Boolean]=Some(true),
                    tail:Option[String]=None
          ): Either[String, Array[String]] = {
    logger(ContainerLogs(id, follow, stdout, stderr, since, timestamps, tail)).run {
      sendForText(
        HttpRequest(path = s"/containers/${id}/logs")
          .queryString(
            Map(
              "follow" -> follow.map(_.toString),
              "stdout" -> stdout.map(_.toString),
              "stderr" -> stderr.map(_.toString),
              "since" -> since.map(_.toString),
              "timestamps" -> timestamps.map(_.toString),
              "tail" -> tail.map(_.toString),
            ).filter { case (k, v) => v.isDefined }.map { case (k, v) => (k, v.get) }
          ),
        responseWrapper = resp =>
          resp.copy(
            headers = resp.headers ::: List(("Content-type", "text/plain"))
          )
      ).map(rawText =>
        rawText
          .split("\\r?\\n")
          .map { line =>
            // [1,0,0,0,0,0,0,109,50,48,50,50,45,49,48]      m
            if (line.length >= 8 && line.charAt(0).toInt == 1) {
              line.substring(8)
            } else {
              line
            }
          }
      )
    }
  }

  /**
   * Cтарт контейнера, контейнер должен быть уже создан [[containerCreate()]]
   * @param containerId контейнер ид или имя
   * @return контейнер запускается и может быть еще не запущен
   */
  def containerStart(containerId:String): Either[String, Unit] = {
    logger(ContainerStart(containerId)).run {
      sendForHttp(
        HttpRequest(path = s"/containers/${containerId}/start", method = "POST")
      ) match {
        case Left(errMessage) =>
          errMessage match {
            case HttpResponse.NO_RESPONSE =>
              Right(())
            case _ =>
              Left(errMessage)
          }
        case Right(response) =>
          response.code match {
            case Some(200) => Right(())
            case Some(304) => Right(()) // already started
            case Some(code) => Left(s"code = $code")
            case None => Left(s"some wrong\n$response")
          }
      }
    }
  }

  /**
   * Остановка конйтейнера
   * @param containerId контейнер ид или имя
   * @return остановка контейнера
   */
  def containerStop(containerId:String): Either[String, Unit] = {
    logger(ContainerStart(containerId)).run {
      sendForHttp(
        HttpRequest(path = s"/containers/${containerId}/stop", method = "POST")
      ) match {
        case Left(errMessage) =>
          errMessage match {
            case HttpResponse.NO_RESPONSE =>
              Right(())
            case _ =>
              Left(errMessage)
          }
        case Right(response) =>
          response.code match {
            case Some(200) => Right(())
            case Some(304) => Right(()) // already started
            case Some(code) => Left(s"code = $code")
            case None => Left(s"some wrong\n$response")
          }
      }
    }
  }

  /**
   * Создание контейнера из образа см [[imageCreate()]]
   * @param createContainerRequest как создать контейнер
   * @param name имя контейнера
   * @param platform платформа, существенно для ос windows
   * @return результат создания
   */
  def containerCreate(
                       createContainerRequest: CreateContainerRequest,
                       name:Option[String]=None,
                       platform:Option[String]=None
                     ): Either[String, CreateContainerResponse] = {
    logger(ContainerCreate(createContainerRequest,name, platform)).run {
      sendForJson[CreateContainerResponse](
        HttpRequest("/containers/create")
          .post()
          .json(createContainerRequest)
          .queryString("name" -> name, "platform" -> platform),
        successHttpCode = r => r.code.exists(c => c >= 200 && c < 300)
      )
    }
  }

  /**
   * Принудительное остановка контейнера (SIGTERM)
   * @param containerId имя или id контейнера
   * @return результат остановки
   */
  def containerKill(containerId:String): Either[String, Unit] = {
    logger(ContainerKill(containerId)).run {
      sendForHttp(HttpRequest(s"/containers/$containerId/kill").post()) match {
        case Left(errMessage) =>
          errMessage match {
            case HttpResponse.NO_RESPONSE =>
              Right(())
            case _ =>
              Left(errMessage)
          }
        case Right(response) =>
          response.code match {
            case Some(200) => Right(())
            case Some(304) => Right(()) // already started
            case Some(code) => Left(s"code = $code")
            case None => Left(s"some wrong\n$response")
          }
      }
    }
  }

  /**
   * Удаление контейнера
   * @param containerId имя или id контейнера
   * @param anonVolumesRemove удалить так же анонимыные тома
   * @param force форсировать удаление, прибивать запущенные контейнеры
   * @param link Remove the specified link associated with the container.
   * @return результат
   */
  def containerRemove(containerId:String, anonVolumesRemove:Option[Boolean]=None, force:Option[Boolean]=None, link:Option[Boolean]=None ): Either[String, Unit] = {
    logger(ContainerRemove(containerId, anonVolumesRemove, force, link)).run {
      sendForHttp(
        HttpRequest(s"/containers/$containerId")
          .delete()
          .queryString("v" -> anonVolumesRemove, "force" -> force, "link" -> link)
      ) match {
        case Left(errMessage) =>
          errMessage match {
            case HttpResponse.NO_RESPONSE =>
              Right(())
            case _ =>
              Left(errMessage)
          }
        case Right(response) =>
          response.code match {
            case Some(200) => Right(())
            case Some(304) => Right(()) // already started
            case Some(code) => Left(s"code = $code")
            case None => Left(s"some wrong\n$response")
          }
      }
    }
  }

  /**
   * Возвращает, какие файлы в файловой системе контейнера были добавлены, удалены или изменены.
   * @param containerId  имя или id контейнера
   * @return список файлов
   */
  def containerFsChanges(containerId:String): Either[String, List[ContainerFileChanges]] =
    logger(ContainerFsChanges(containerId)).run {
      sendForJson[List[model.ContainerFileChanges]](
        HttpRequest(s"/containers/$containerId/changes").get()
      )
    }

  /**
   * Переименование контейнера
   * @param containerId имя или id контейнера
   * @param newName новое имя
   * @return результат
   */
  def containerRename(containerId:String, newName:String): Either[String, Unit] =
    logger(ContainerRename(containerId,newName)).run {
      sendForHttp(
        HttpRequest(s"/containers/$containerId/rename")
          .delete()
          .queryString("id" -> containerId, "name" -> newName)
      ) match {
        case Left(errMessage) =>
          errMessage match {
            case HttpResponse.NO_RESPONSE =>
              Right(())
            case _ =>
              Left(errMessage)
          }
        case Right(response) =>
          response.code match {
            case Some(200) => Right(())
            case Some(304) => Right(()) // already started
            case Some(code) => Left(s"code = $code")
            case None => Left(s"some wrong\n$response")
          }
      }
    }

  //#endregion
  //#region image task

  def images(): Either[String, List[Image]] =
    logger(Images()).run {
      sendForJson[List[model.Image]](
        HttpRequest("/images/json").get()
      )
    }

  def imageInspect(imageId:String): Either[String, model.ImageInspect] =
    logger(Logger.ImageInspect(imageId)).run {
      sendForJson[model.ImageInspect](
        HttpRequest(s"/images/${imageId}/json").get()
      )
    }

  def imageCreate( fromImage:Option[String] = None,
                   fromSrc:Option[String] = None,
                   repo:Option[String] = None,
                   tag:Option[String] = None,
                   message:Option[String] = None,
                   platform:Option[String] = None
                 )(progress:ImagePullStatusEntry=>Unit):Unit =
  {
    val logReq = logger(Logger.ImageCreate(fromImage, fromSrc, repo, tag, message, platform))
    var logEvents = List[model.ImagePullStatusEntry]()

    lazy val byte2charDecoder: Decoder.Byte2Char = Decoder.Byte2Char(StandardCharsets.UTF_8.newDecoder())
    lazy val char2json = Decoder.Char2JsonEntry()
    lazy val decoder = char2json.compose(byte2charDecoder)

    stream(
      HttpRequest("/images/create").post()
        .queryString(
          "fromImage"->fromImage,
          "fromSrc"->fromSrc,
          "repo"->repo,
          "tag"->tag,
          "message"->message,
          "platform"->platform,
        )
    ) { ev =>
      ev match {
        case Event.Error(pid,string) => ()
        case Event.FirstLine(pid,string) => ()
        case Event.Header(pid,name,value) => ()
        case Event.HeaderEnd(pid) => ()
        case Event.DataEnd(pid) => ()
        case Event.Data(pid,bytes) =>
          decoder.accept(bytes)
          decoder.fetch.foreach { jsEntryString =>
            jsEntryString.trim.jsonAs[ImagePullStatusEntry].foreach { ent =>
              logEvents = ent :: logEvents
              progress(ent)
            }
          }
      }
      HttpResponseStream.Behavior.Continue
    }

    logReq.success(logEvents.reverse)
  }

  /**
   * Remove an image, along with any untagged parent images that were referenced by that image.
   * Images can't be removed if they have descendant images, are being used by a running container or are being used by a build.
   * @param nameOrId Image name or ID
   * @param force Remove the image even if it is being used by stopped containers or has other tags
   * @param noprune Do not delete untagged parent images
   */
  def imageRemove(nameOrId:String,force:Option[Boolean]=None,noprune:Option[Boolean]=None): Either[String, List[model.ImageRemove]] =
    logger(Logger.ImageRemove(nameOrId, force, noprune)).run {
      sendForJson[List[model.ImageRemove]](
        HttpRequest(s"/images/${nameOrId}").delete().queryString("force" -> force, "noprune" -> noprune)
      )
    }

  def imageTag(nameOrId:String,repo:Option[String]=None,tag:Option[String]=None): Either[String, Unit] =
    logger(Logger.ImageTag(nameOrId, repo, tag)).run {
      sendForHttp(
        HttpRequest(path = s"/images/${nameOrId}/tag")
          .post()
          .queryString("repo" -> repo, "tag" -> tag)
      ) match {
        case Left(errMessage) =>
          errMessage match {
            case HttpResponse.NO_RESPONSE =>
              Right(())
            case _ =>
              Left(errMessage)
          }
        case Right(response) =>
          response.code match {
            case Some(200) => Right(())
            case Some(201) => Right(())
            case Some(204) => Right(())
            case Some(code) => Left(s"code = $code\n${response.text}")
            case None => Left(s"some wrong\n$response")
          }
      }
    }


  def imageHistory(nameOrId:String): Either[String, List[ImageHistory]] = {
    logger(Logger.ImageHistory(nameOrId)).run {
      sendForJson[List[model.ImageHistory]](HttpRequest(s"/images/$nameOrId/history"))
    }
  }
  //#endregion
}

object DockerClient {
  lazy val idSeq = new AtomicInteger(0)

  def unixSocket(path:String)(implicit
                              httpLogger: HttpLogger,
                              socketLogger: SocketLogger,
                              logger: Logger
  ):DockerClient =
    DockerClient(
      SocketChannel.open(UnixDomainSocketAddress.of(path)),
      ()=>SocketChannel.open(UnixDomainSocketAddress.of(path))
    )(httpLogger = httpLogger,socketLogger = socketLogger,logger = logger)
}
