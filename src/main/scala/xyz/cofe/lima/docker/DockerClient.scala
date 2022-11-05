package xyz.cofe.lima.docker

import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.HttpResponseStream.Event
import xyz.cofe.lima.docker.http._
import xyz.cofe.lima.docker.http.errors.HttpError
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.docker.log.Logger._
import xyz.cofe.lima.docker.model.{ImageHistory, _}

import java.net.UnixDomainSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger
import java.util.concurrent.locks.{Lock, ReentrantLock}

case class DockerClient( socketChannel: SocketChannel,
                         openSocket: ()=>SocketChannel,
                         socketReadTimings: SocketReadTimings = SocketReadTimings(),
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
      socketReadTimings,
      socketLock = new ReentrantLock(),
      clientId = DockerClient.idSeq.incrementAndGet()
    )(httpLogger,socketLogger,logger)

  def withLogger(httpLogger: HttpLogger):DockerClient =
    DockerClient(
      socketChannel,
      openSocket,
      socketReadTimings,
      socketLock
    )(httpLogger,socketLogger,logger)

  def withLogger(logger: Logger):DockerClient =
    DockerClient(
      socketChannel,
      openSocket,
      socketReadTimings,
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

  private lazy val httpClient = HttpClient(socketChannel,socketReadTimings,socketLock)
  private def http(request:HttpRequest) = httpClient.http(request)

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
    logger(Logger.Containers(all, limit, size)).run {
      http(HttpRequest(path = "/containers/json").queryString(
        "all" -> all,
        "limit" -> limit,
        "size" -> size
      ).get()).validStatusCode(200).json[List[model.ContainerStatus]].left.map(_.message)
    }
  }

  /**
   * Получение информации о контейнере
   * @param id идентификатор контейнера
   * @return инфа
   */
  def containerInspect(id:String): Either[String, model.ContainerInspect] =
    logger(Logger.ContainerInspect(id)).run {
      http(HttpRequest(s"/containers/${id}/json").get()).validStatusCode(200).json[model.ContainerInspect].left.map { err =>
        //println(err)
        err.message
      }
    }

  /**
   * Получение информации о процессах внутри контейнера
   * @param id идентификатор контейнера
   * @return процессы
   */
  def containerProcesses(id:String): Either[String, model.Top] =
    logger(ContainerProcesses(id)).run {
      http(HttpRequest(s"/containers/${id}/top").get()).validStatusCode(200).json[model.Top].left.map(_.message)
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
   * @param follow bla
   * @param stdout bla
   * @param stderr bla
   * @param since bla
   * @param timestamps bla
   * @param tail bla
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
      http(
        HttpRequest(s"/containers/${id}/logs")
          .get()
          .queryString(
            "follow"->follow,
            "stdout" -> stdout,
            "stderr" -> stderr,
            "since" -> since,
            "timestamps" -> timestamps,
            "tail" -> tail
          )
      )
        .validStatusCode(200)
        .map( resp => resp.copy(headers = resp.headers ::: List(("Content-type", "text/plain"))))
        .text.map(rawText =>
        rawText
          .split("\\r?\\n")
          .map { line =>
            // [1,0,0,0,0,0,0,109,50,48,50,50,45,49,48]      m
            if (line.length >= 8 && line.charAt(0).toInt == 1) {
              line.substring(8)
            } else {
              line
            }
          }
      ).left.map(_.message)
    }
  }

  /**
   * Cтарт контейнера, контейнер должен быть уже создан [[containerCreate()]]
   * @param containerId контейнер ид или имя
   * @return контейнер запускается и может быть еще не запущен
   */
  def containerStart(containerId:String): Either[String, Unit] = {
    logger(ContainerStart(containerId)).run {
      http(
        HttpRequest(s"/containers/${containerId}/start").post()
      ).validStatusCode(204)
        .left
        .map { e => e.message }
        .map { _ => () }
    }
  }

  /**
   * Остановка конйтейнера
   * @param containerId контейнер ид или имя
   * @return остановка контейнера
   */
  def containerStop(containerId:String): Either[String, Unit] = {
    logger(ContainerStart(containerId)).run {
      http(
        HttpRequest(s"/containers/${containerId}/stop").post()
      ).validStatusCode(204)
        .left
        .map { e => e.message }
        .map { _ => () }
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
      http(
        HttpRequest("/containers/create")
        .post()
        .json(createContainerRequest)
        .queryString("name" -> name, "platform" -> platform)
      )
        .validStatusCode(200)
        .json[CreateContainerResponse].left.map(_.message)
    }
  }

  /**
   * Принудительное остановка контейнера (SIGTERM)
   * @param containerId имя или id контейнера
   * @return результат остановки
   */
  def containerKill(containerId:String): Either[String, Unit] = {
    logger(ContainerKill(containerId)).run {
      http(
        HttpRequest((s"/containers/$containerId/kill")).post()
      ).validStatusCode(200,204,304).left.map(_.message).map(_ => ())
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
      http(
        HttpRequest((s"/containers/$containerId")).delete()
          .queryString("v"->anonVolumesRemove, "force"->force, "link"->link)
      ).validStatusCode(200, 204, 304).left.map(_.message).map(_ => ())
    }
  }

  /**
   * Возвращает, какие файлы в файловой системе контейнера были добавлены, удалены или изменены.
   * @param containerId  имя или id контейнера
   * @return список файлов
   */
  def containerFsChanges(containerId:String): Either[String, List[ContainerFileChanges]] =
    logger(ContainerFsChanges(containerId)).run {
      http(
        HttpRequest(s"/containers/$containerId/changes").get()
      )
        .validStatusCode(200)
        .json[List[ContainerFileChanges]]
        .left.map(_.message)
    }

  /**
   * Переименование контейнера
   * @param containerId имя или id контейнера
   * @param newName новое имя
   * @return результат
   */
  def containerRename(containerId:String, newName:String): Either[String, Unit] =
    logger(ContainerRename(containerId,newName)).run {
      http(
        HttpRequest(s"/containers/$containerId/rename")
          .delete()
          .queryString("id" -> containerId, "name" -> newName)
      )
        .validStatusCode(204)
        .map(_ => ())
        .left.map(_.message)
    }

  //#endregion
  //#region image task

  /**
   * Получение списка образов
   * @return список образов
   */
  def images(): Either[String, List[Image]] =
    logger(Images()).run {
      http(HttpRequest("/images/json").get())
        .validStatusCode(200)
        .json[List[model.Image]]
        .left.map(_.message)
    }

  /**
   * Получение детальной информации по образу
   * @param imageId ид образа
   * @return информация по образу
   */
  def imageInspect(imageId:String): Either[String, model.ImageInspect] =
    logger(Logger.ImageInspect(imageId)).run {
      http(HttpRequest(s"/images/${imageId}/json").get())
        .validStatusCode(200)
        .json[model.ImageInspect]
        .left.map(_.message)
    }

  /**
   * Получение/создание образа из реестра или импорт образа
   * @param fromImage Имя образа для извлечения. Имя может включать тег или дайджест. Этот параметр можно использовать только при извлечении(pull) образа. Вытягивание отменяется, если соединение HTTP закрыто.
   * @param fromSrc Источник для импорта. Значение может быть URL-адресом, по которому можно получить образ, или - для чтения образа из тела запроса. Этот параметр можно использовать только при импорте образа.
   * @param repo Имя репозитория, присвоенное образу при его импорте. Может включать тег. Этот параметр можно использовать только при импорте образа.
   * @param tag Тег или подпись (digest). Если пусто при извлечении изображения, это приводит к извлечению всех тегов для данного образа.
   * @param message Set commit message for imported image.
   * @param platform Platform in the format os [/arch [/variant] ]
   * @param progress куда слать прогресс
   */
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

    val request =
      HttpRequest("/images/create").post()
        .queryString(
          "fromImage" -> fromImage,
          "fromSrc" -> fromSrc,
          "repo" -> repo,
          "tag" -> tag,
          "message" -> message,
          "platform" -> platform,
        )

    httpClient.stream(request) { ev =>
      ev match {
        case Event.Data(pid, bytes) =>
          decoder.accept(bytes)
          decoder.fetch.foreach { jsEntryString =>
            jsEntryString.trim.jsonAs[ImagePullStatusEntry].foreach { ent =>
              logEvents = ent :: logEvents
              progress(ent)
            }
          }
        case _ => ()
      }
      HttpResponseStream.Behavior.Continue
    }

    val r : Either[String,List[ImagePullStatusEntry]] = Right(logEvents.reverse)
    logReq.run(r)
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
      http(
        HttpRequest(s"/images/${nameOrId}").delete().queryString("force" -> force, "noprune" -> noprune)
      ).validStatusCode(200)
        .json[List[model.ImageRemove]]
        .left.map(_.message)
    }

  def imageTag(nameOrId:String,repo:Option[String]=None,tag:Option[String]=None): Either[String, Unit] =
    logger(Logger.ImageTag(nameOrId, repo, tag)).run {
      http(
        HttpRequest(path = s"/images/${nameOrId}/tag")
          .post()
          .queryString("repo" -> repo, "tag" -> tag)
      )
        .validStatusCode(201)
        .left.map(_.message)
        .map(_ => ())
    }


  def imageHistory(nameOrId:String): Either[String, List[ImageHistory]] = {
    logger(Logger.ImageHistory(nameOrId)).run {
      http(HttpRequest(s"/images/$nameOrId/history").get())
        .validStatusCode(200)
        .json[List[ImageHistory]]
        .left.map(_.message)
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
