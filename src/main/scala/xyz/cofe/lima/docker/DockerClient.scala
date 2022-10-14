package xyz.cofe.lima.docker

import xyz.cofe.lima.docker.http.{Decoder, HttpLogger, HttpRequest, HttpResponse, HttpResponseReader, HttpResponseStream, SocketChannelSupplier, SocketLogger}

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.HttpResponseStream.Event
import xyz.cofe.lima.docker.model.{CreateContainerRequest, CreateContainerResponse, Image, ImageInspect, ImagePullStatusEntry, ImageRemove}

import java.net.UnixDomainSocketAddress

case class DockerClient( socketChannel: SocketChannel,
                         sourceTimeout:Long=1000L,
                         readTimeout:  Long=1000L*30L,
                         cpuThrottling:Long=1,
                         streamReadTimeout:Long=(-1),
                         streamSourceTimeout:Long=1000L*30L
                       )
                       (implicit
                        httpLogger: HttpLogger,
                        socketLogger: SocketLogger
                       )
{
  socketChannel.configureBlocking(false)

  private def sendRequest(request:HttpRequest):Unit = {
    val headerBlock =
      ((request.method + " " + request.path + " " + request.proto + "\n") +
        ("HOST: "+request.host+"\n")+
        ("User-Agent: "+request.`User-Agent`+"\n")+
        ("Accept: "+request.Accept+"\n")+
        (request.otherHeaders.map { case (k,v) => k+": "+v }.mkString("\n"))+
        "\n"
        ).getBytes(StandardCharsets.UTF_8)

    val sendBB = ByteBuffer.allocate(headerBlock.size + request.body.size)
    sendBB.put(headerBlock)

    val bodyArr = request.body.toArray
    sendBB.put(bodyArr)
    sendBB.flip()

    socketLogger.send(sendBB)
    socketChannel.write(sendBB)
  }

  def stream(request:HttpRequest)(consumer:HttpResponseStream.Event=>HttpResponseStream.Behavior):Unit = {
    sendRequest(request)
    HttpResponseStream(
      SocketChannelSupplier(socketChannel),
      sourceTimeout = streamSourceTimeout,
      readTimeout = streamReadTimeout,
      cpuThrottling = cpuThrottling
    ).read(consumer)
  }

  def sendForHttp(request:HttpRequest):Either[String,HttpResponse] = {
    sendRequest(request)

    val socketChannelSupplier = SocketChannelSupplier(socketChannel)
    HttpResponseReader(
      socketChannelSupplier,
      sourceTimeout = sourceTimeout,
      readTimeout = readTimeout,
      cpuThrottling = cpuThrottling
    ).read
  }

  def sendForText(
    request:HttpRequest,
    responseWrapper:HttpResponse=>HttpResponse = r=>r
  ):Either[String,String] = {
    for {
      _ <- Right(httpLogger.send(request))
      response0 <- sendForHttp(request)
      response = responseWrapper(response0)
      _ <- Right(httpLogger.receive(response))
      _ <- if (response.isOk) {
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

  def sendForJson[A:JsonReader](
                          request:HttpRequest,
                          responseWrapper:HttpResponse=>HttpResponse = r=>r
                        ):Either[String,A] = {
    for {
      _ <- Right(httpLogger.send(request))
      response0 <- sendForHttp(request)
      response = responseWrapper(response0)
      _ <- Right(httpLogger.receive(response))

      _ <- if (response.isOk) {
        Right(response)
      } else {
        Left(s"response not ok\ncode = ${response.code}\ntext = ${response.text}")
      }
      text <- response.text.map(
        s => Right(s)
      ).getOrElse(
        Left("text response not available")
      )
      res <- text.jsonAs[A].left.map( err => err.getMessage )
    } yield res
  }

  /**
   * Получение списка контейнеров
   * @param all запросить все контейнеры
   * @param limit ограничить размер выборки
   * @param size получить информацию о размере контейнера
   * @return контейнеры
   */
  def containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false)
  : Either[String, List[model.ContainerStatus]] = {
    val q = Map("all"->all.toString) ++
      (limit.map(l=>Map("limit" -> l.toString)).getOrElse(Map())) ++
      (if(size)Map("size"->size.toString)else(Map()))

    sendForJson[List[model.ContainerStatus]](
      HttpRequest(path = "/containers/json").queryString(q)
    )
  }

  /**
   * Получение информации о контейнере
   * @param id идентификатор контейнера
   * @return инфа
   */
  def containerInspect(id:String): Either[String, model.ContainerInspect] =
    sendForJson[model.ContainerInspect](
      HttpRequest(path = s"/containers/${id}/json")
    )

  /**
   * Получение информации о процессах внутри контейнера
   * @param id идентификатор контейнера
   * @return процессы
   */
  def containerProcesses(id:String): Either[String, model.Top] =
    sendForJson[model.Top](
      HttpRequest(path = s"/containers/${id}/top")
    )

  /**
   * Получение логов контейнера
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
    sendForText(
      HttpRequest(path = s"/containers/${id}/logs")
        .queryString(
          Map(
            "follow"->follow.map(_.toString),
            "stdout"->stdout.map(_.toString),
            "stderr"->stderr.map(_.toString),
            "since"->since.map(_.toString),
            "timestamps"->timestamps.map(_.toString),
            "tail"->tail.map(_.toString),
          ).filter { case (k,v) => v.isDefined }.map { case (k,v) => (k,v.get) }
        ),
      responseWrapper = resp =>
        resp.copy(
          headers = resp.headers ::: List(("Content-type","text/plain"))
        )
    ).map( rawText =>
      rawText
        .split("\\r?\\n")
        .map { line =>
          // [1,0,0,0,0,0,0,109,50,48,50,50,45,49,48]      m
          if( line.length>=8 && line.charAt(0).toInt == 1 ){
            line.substring(8)
          }else{
            line
          }
        }
    )
  }

  def containerStart(containerId:String): Either[String, Unit] = {
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
  def containerStop(containerId:String): Either[String, Unit] = {
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

  def containerCreate(
                       createContainerRequest: CreateContainerRequest,
                       name:Option[String]=None,
                       platform:Option[String]=None
                     ): Either[String, CreateContainerResponse] = {
    sendForJson[CreateContainerResponse](
      HttpRequest(method = "POST", path = "containers/create")
        .json(createContainerRequest)
        .queryString("name"->name,"platform"->platform)
    )
  }

  def containerKill(containerId:String): Either[String, Unit] = {
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

  def containerRemove(containerId:String, anonVolumesRemove:Option[Boolean]=None, force:Option[Boolean]=None, link:Option[Boolean]=None ): Either[String, Unit] = {
    sendForHttp(
      HttpRequest(s"/containers/$containerId")
        .delete()
        .queryString( "v"->anonVolumesRemove, "force"->force, "link"->link )
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

  def images(): Either[String, List[Image]] = sendForJson[List[model.Image]](
    HttpRequest("/images/json").get()
  )

  def imageInspect(imageId:String): Either[String, ImageInspect] = sendForJson[model.ImageInspect](
    HttpRequest(s"/images/${imageId}/json").get()
  )

  def imageCreate( fromImage:Option[String] = None,
                   fromSrc:Option[String] = None,
                   repo:Option[String] = None,
                   tag:Option[String] = None,
                   message:Option[String] = None,
                   platform:Option[String] = None
                 )(progress:ImagePullStatusEntry=>Unit):Unit =
  {
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
        case Event.Error(string) => ()
        case Event.FirstLine(string) => ()
        case Event.Header(name, value) => ()
        case Event.HeaderEnd => ()
        case Event.DataEnd => ()
        case Event.Data(bytes) =>
          decoder.accept(bytes)
          decoder.fetch.foreach { jsEntryString =>
            jsEntryString.trim.jsonAs[ImagePullStatusEntry].foreach { ent =>
              progress(ent)
            }
          }
      }
      HttpResponseStream.Behavior.Continue
    }
  }

  /**
   * Remove an image, along with any untagged parent images that were referenced by that image.
   * Images can't be removed if they have descendant images, are being used by a running container or are being used by a build.
   * @param nameOrId Image name or ID
   * @param force Remove the image even if it is being used by stopped containers or has other tags
   * @param noprune Do not delete untagged parent images
   */
  def imageRemove(nameOrId:String,force:Option[Boolean]=None,noprune:Option[Boolean]=None): Either[String, List[ImageRemove]] =
    sendForJson[List[model.ImageRemove]](
      HttpRequest(s"/images/${nameOrId}").delete().queryString("force"->force,"noprune"->noprune)
    )

  def imageTag(nameOrId:String,repo:Option[String]=None,tag:Option[String]=None): Either[String, Unit] = {
    sendForHttp(
      HttpRequest(path = s"/images/${nameOrId}/tag")
        .post()
        .queryString("repo"->repo,"tag"->tag)
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
          case Some(204) => Right(())
          case Some(code) => Left(s"code = $code")
          case None => Left(s"some wrong\n$response")
        }
    }
  }
}

object DockerClient {
  def unixSocket(path:String)(implicit
                              httpLogger: HttpLogger,
                              socketLogger: SocketLogger
  ):DockerClient =
    DockerClient(SocketChannel.open(UnixDomainSocketAddress.of(path)))
}
