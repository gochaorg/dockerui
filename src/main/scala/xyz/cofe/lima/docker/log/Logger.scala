package xyz.cofe.lima.docker.log

import tethys._
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.jackson._
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.docker.errors.DockerError
import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.model.{CreateContainerRequest, Image}
import xyz.cofe.lima.store.json._
import xyz.cofe.lima.thread.ThreadID

import java.time.LocalDateTime
import scala.util.Either

/**
 * Логирование работы DockerClient
 *
 * '''Использование'''
 *
 * есть где то в клиенте {{{implicit logger: Logger}}}
 *
 * и его использование в коде клиента
 *
 * <pre>
 * def containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false)
 *   : Either[errors.DockerError, List[model.ContainerStatus] ] = {
 *     logger(Logger.Containers(all, limit, size)).run {
 *       http(HttpRequest(path = "/containers/json").queryString(
 *       "all" -> all,
 *       "limit" -> limit,
 *       "size" -> size
 *     ).get())
 *       .expect
 *       .fail(400, errors.BadRequest(_))
 *       .json[List[model.ContainerStatus] ](200)
 *   }
 * }
 * </pre>
 *
 * А направлять лог событий так
 *
 * {{{
 * val dc = DockerClient.unixSocket(str)
 *  .withLogger(
 *    Logger.JsonToWriter(
 *      AppendableFile(
 *        PathPattern.escape(AppHome.directory) ++
 *        PathPattern.parse(Path.of("log/dockerClient/{yyyy}-{MM}/{dd}/{hh}-{mm}-{ss}.stream.json")),
 *        limitSizePerFile = Some(1024L*512L)
 *      )
 *    )
 *  )
 * }}}
 */
trait Logger {
  import Logger._
  def apply[M<:MethodCall](op:M)(implicit jw:JsonWriter[M],jwr:JsonWriter[M#RESULT]):CallCatcher[M] = new CallCatcher(op)
}

object Logger {
  trait ClientId {
    def clientId: Int
  }

  class CallCatcher[M <: MethodCall](val op:M) {
    def run[E](someResult: Either[E, M#RESULT])(implicit jwe:JsonWriter[E]): Either[E, M#RESULT] = someResult
  }

  sealed trait MethodCall {
    type RESULT
    def parameters: (String, Product)
    def resultToJson(result: RESULT): String
  }

  //#region Default logger

  implicit val defaultLogger: Logger = new Logger {}

  //#endregion
  //#region errorCapture logger

  def filterLogger(logger:Logger)( filter:(_ >: MethodCall,Either[_, _])=>Boolean ):Logger = new Logger {
    class FilterCatcher[M <: MethodCall](op:M, op1:CallCatcher[M]
                                        )(implicit jw: JsonWriter[M], jwr: JsonWriter[M#RESULT]) extends CallCatcher(op) {
      override def run[E](someResult: Either[E, M#RESULT])(implicit jwe: JsonWriter[E]): Either[E, M#RESULT] = {
        if( filter(op,someResult) ){
          op1.run(someResult)
        }
        super.run(someResult)
      }
    }

    override def apply[M <: MethodCall](op: M)(implicit jw: JsonWriter[M], jwr: JsonWriter[M#RESULT]): CallCatcher[M] =
      new FilterCatcher(op, logger(op))
  }

  def failLogger(logger:Logger):Logger = filterLogger(logger) ( (_:Any, res) => res.isLeft )

  //#endregion
  //#region joinLoggers

  def joinLoggers(left:Logger, right:Logger):Logger = new Logger {
    class JoinCallCatcher[M <: MethodCall](leftCc:CallCatcher[M], rightCc:CallCatcher[M],op:M)
                                          (implicit jw: JsonWriter[M], jwr: JsonWriter[M#RESULT])
      extends CallCatcher(op)
    {
      override def run[E](someResult: Either[E, M#RESULT])(implicit jwe: JsonWriter[E]): Either[E, M#RESULT] = {
        leftCc.run(someResult)
        rightCc.run(someResult)
        someResult
      }
    }

    override def apply[M <: MethodCall](op: M)(implicit jw: JsonWriter[M], jwr: JsonWriter[M#RESULT]): CallCatcher[M] = {
      val leftCc = left.apply(op)
      val rightCc = right.apply(op)
      new JoinCallCatcher[M](leftCc, rightCc, op)
    }
  }
  //#endregion
  //#region Json logger

  /**
   * Логируемое событие
   * @tparam ARGS метод клиента и его параметры
   */
  sealed trait LogEvent[ARGS,ERR] {
    def args:ARGS
  }

  object LogEvent {
    implicit val reader:JsonReader[LogEvent[_,_]] = JsonLogEventReader.reader
  }

  /**
   * Логируемое событие - успешный вызов метода
   * @param threadId поток
   * @param beginTime время начала вызова метода клиента
   * @param endTime время завершения вызова метода клиента
   * @param args метод и его параметры
   * @param result результат вызова
   * @tparam ARGS метод клиента и его параметры
   * @tparam RES результат вызова
   */
  case class SuccEvent[ARGS <: MethodCall:JsonWriter,RES:JsonWriter](threadId:ThreadID, beginTime:LocalDateTime, endTime:LocalDateTime, args:ARGS, result:RES) extends LogEvent[ARGS,Unit]
  object SuccEvent {
    implicit def writer[A<:MethodCall:JsonWriter,R:JsonWriter]:JsonWriter[SuccEvent[A,R]] = classWriter[SuccEvent[A,R]] ++ jsonWriter[SuccEvent[A,R]]
  }

  /**
   * Логируемое событие - ошибочный вызов метода
   *
   * @param threadId          поток
   * @param beginTime         время начала вызова метода клиента
   * @param endTime           время завершения вызова метода клиента
   * @param args              метод и его параметры
   * @param error             ошбика
   * @tparam ARGS метод клиента и его параметры
   * @tparam ERR тип ошибки
   */
  case class FailEvent[ARGS:JsonWriter,ERR:JsonWriter](threadId:ThreadID, beginTime:LocalDateTime, endTime:LocalDateTime, args:ARGS, error:ERR) extends LogEvent[ARGS,ERR]
  object FailEvent {
    implicit def writer[A:JsonWriter,R:JsonWriter]:JsonWriter[FailEvent[A,R]] = classWriter[FailEvent[A,R]] ++ jsonWriter[FailEvent[A,R]]
  }

  /**
   * Запись событий в форме Json в поток
   * @param out поток куда записываются события
   */
  class JsonToWriter(out:java.lang.Appendable) extends Logger {
    class JsonCallCatcher[M <: MethodCall](op:M, begin:LocalDateTime)(implicit jw:JsonWriter[M],jwr:JsonWriter[M#RESULT]) extends CallCatcher(op) {
      override def run[E](someResult: Either[E, M#RESULT])(implicit jwe:JsonWriter[E]): Either[E, M#RESULT] = {
        someResult match {
          case Left(error) =>
            out.append( FailEvent(ThreadID.current, begin, LocalDateTime.now(), op, error).asJson ).append(System.lineSeparator())
          case Right(sucResult) =>
            out.append( SuccEvent(ThreadID.current, begin, LocalDateTime.now(), op, sucResult).asJson ).append(System.lineSeparator())
        }
        someResult
      }
    }

    override def apply[M <: MethodCall](op: M)(implicit jw:JsonWriter[M],jwr:JsonWriter[M#RESULT]): CallCatcher[M] =
      (new JsonCallCatcher[M](op, LocalDateTime.now())).asInstanceOf[CallCatcher[M]]
  }

  object JsonToWriter {
    def apply(out:java.lang.Appendable):JsonToWriter = new JsonToWriter(out)
  }

  //#endregion

  //#region log events

  implicit def arrayOfString2Json: JsonWriter[Array[String]] = (value: Array[String], tokenWriter: TokenWriter) => {
    tokenWriter.writeArrayStart()
    value.foreach(itm => {
      tokenWriter.writeString(itm)
    })
    tokenWriter.writeArrayEnd()
  }
  implicit def json2arrayOfString: JsonReader[Array[String]] = {
    implicitly[JsonReader[List[String]]].map[Array[String]] { listArr => listArr.toArray }
  }

  implicit def unit2Json: JsonWriter[Unit] = (_: Unit, tw: TokenWriter) => { tw.writeNull() }
  implicit def json2unit: JsonReader[Unit] = new JsonReader[Unit] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Unit = {
      if(it.currentToken().isNullValue){
        it.next()
      }else{
        throw new Error("expect null")
      }
    }
  }

  abstract class MethodWithParams[A:JsonWriter] extends Product with MethodCall {
    import tethys._
    import tethys.jackson._
    override type RESULT = A
    override def parameters: (String, Product) = (this.getClass.getSimpleName,this)
    override def resultToJson(result:RESULT):String = result.asJson
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containers]]
   * @param all параметры метода
   * @param limit параметры метода
   * @param size параметры метода
   */
  case class Containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false) extends MethodWithParams[List[model.ContainerStatus]]
  object Containers {
    implicit val reader: JsonReader[Containers] = jsonReader[Containers]
    implicit val writer: JsonWriter[Containers] = classWriter[Containers] ++ jsonWriter[Containers]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerInspect]]
   */
  case class ContainerInspect(id:String) extends MethodWithParams[model.ContainerInspect]
  object ContainerInspect {
    implicit val reader: JsonReader[ContainerInspect] = jsonReader[ContainerInspect]
    implicit val writer: JsonWriter[ContainerInspect] = classWriter[ContainerInspect] ++ jsonWriter[ContainerInspect]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerProcesses]]
   */
  case class ContainerProcesses(id:String) extends MethodWithParams[model.Top]
  object ContainerProcesses {
    implicit val reader: JsonReader[ContainerProcesses] = jsonReader[ContainerProcesses]
    implicit val writer: JsonWriter[ContainerProcesses] = classWriter[ContainerProcesses] ++ jsonWriter[ContainerProcesses]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerLogs]]
   */
  case class ContainerLogs(id:String,
                           follow:Option[Boolean]=None,
                           stdout:Option[Boolean]=Some(true),
                           stderr:Option[Boolean]=None,
                           since:Option[Long]=None,
                           timestamps:Option[Boolean]=Some(true),
                           tail:Option[String]=None) extends MethodWithParams[Array[String]]
  object ContainerLogs {
    implicit val reader: JsonReader[ContainerLogs] = jsonReader[ContainerLogs]
    implicit val writer: JsonWriter[ContainerLogs] = classWriter[ContainerLogs] ++ jsonWriter[ContainerLogs]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerStart]]
   */
  case class ContainerStart(containerId:String) extends MethodWithParams[Unit]
  object ContainerStart {
    implicit val reader: JsonReader[ContainerStart] = jsonReader[ContainerStart]
    implicit val writer: JsonWriter[ContainerStart] = classWriter[ContainerStart] ++ jsonWriter[ContainerStart]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerStop]]
   */
  case class ContainerStop(containerId:String) extends MethodWithParams[Unit]
  object ContainerStop {
    implicit val reader: JsonReader[ContainerStop] = jsonReader[ContainerStop]
    implicit val writer: JsonWriter[ContainerStop] = classWriter[ContainerStop] ++ jsonWriter[ContainerStop]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerCreate]]
   */
  case class ContainerCreate(
                              createContainerRequest: CreateContainerRequest,
                              name:Option[String]=None,
                              platform:Option[String]=None
                            ) extends MethodWithParams[model.CreateContainerResponse]
  object ContainerCreate {
    implicit val reader: JsonReader[ContainerCreate] = jsonReader[ContainerCreate]
    implicit val writer: JsonWriter[ContainerCreate] = classWriter[ContainerCreate] ++ jsonWriter[ContainerCreate]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerKill]]
   */
  case class ContainerKill(containerId:String) extends MethodWithParams[Unit]
  object ContainerKill {
    implicit val reader: JsonReader[ContainerKill] = jsonReader[ContainerKill]
    implicit val writer: JsonWriter[ContainerKill] = classWriter[ContainerKill] ++ jsonWriter[ContainerKill]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerRemove]]
   */
  case class ContainerRemove(containerId:String,
                             anonVolumesRemove:Option[Boolean]=None,
                             force:Option[Boolean]=None,
                             link:Option[Boolean]=None ) extends MethodWithParams[Unit]
  object ContainerRemove {
    implicit val reader: JsonReader[ContainerRemove] = jsonReader[ContainerRemove]
    implicit val writer: JsonWriter[ContainerRemove] = classWriter[ContainerRemove] ++ jsonWriter[ContainerRemove]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerFsChanges]]
   */
  case class ContainerFsChanges(containerId:String) extends MethodWithParams[List[model.ContainerFileChanges]]
  object ContainerFsChanges {
    implicit val reader: JsonReader[ContainerFsChanges] = jsonReader[ContainerFsChanges]
    implicit val writer: JsonWriter[ContainerFsChanges] = classWriter[ContainerFsChanges] ++ jsonWriter[ContainerFsChanges]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.containerRename]]
   */
  case class ContainerRename(containerId:String, newName:String) extends MethodWithParams[Unit]
  object ContainerRename {
    implicit val reader: JsonReader[ContainerRename] = jsonReader[ContainerRename]
    implicit val writer: JsonWriter[ContainerRename] = classWriter[ContainerRename] ++ jsonWriter[ContainerRename]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.images]]
   */
  case class Images() extends MethodWithParams[List[Image]]
  object Images {
    implicit val reader: JsonReader[Images] = jsonReader[Images]
    implicit val writer: JsonWriter[Images] = classWriter[Images] ++ jsonWriter[Images]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.imageRemove]]
   */
  case class ImageRemove(nameOrId:String,
                         force:Option[Boolean]=None,
                         noprune:Option[Boolean]=None) extends MethodWithParams[List[model.ImageRemove]]
  object ImageRemove {
    implicit val reader: JsonReader[ImageRemove] = jsonReader[ImageRemove]
    implicit val writer: JsonWriter[ImageRemove] = classWriter[ImageRemove] ++ jsonWriter[ImageRemove]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.imageTag]]
   */
  case class ImageTag(nameOrId:String,
                      repo:Option[String]=None,
                      tag:Option[String]=None) extends MethodWithParams[Unit]
  object ImageTag {
    implicit val reader: JsonReader[ImageTag] = jsonReader[ImageTag]
    implicit val writer: JsonWriter[ImageTag] = classWriter[ImageTag] ++ jsonWriter[ImageTag]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.imageTag]]
   */
  case class ImageHistory(nameOrId:String) extends MethodWithParams[List[model.ImageHistory]]
  object ImageHistory {
    implicit val reader: JsonReader[ImageHistory] = jsonReader[ImageHistory]
    implicit val writer: JsonWriter[ImageHistory] = classWriter[ImageHistory] ++ jsonWriter[ImageHistory]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.imageInspect]]
   */
  case class ImageInspect(nameOrId:String) extends MethodWithParams[model.ImageInspect]
  object ImageInspect {
    implicit val reader: JsonReader[ImageInspect] = jsonReader[ImageInspect]
    implicit val writer: JsonWriter[ImageInspect] = classWriter[ImageInspect] ++ jsonWriter[ImageInspect]
  }

  /**
   * Вызов метода [[xyz.cofe.lima.docker.DockerClient.imageCreate]]
   */
  case class ImageCreate(fromImage: Option[String] = None,
                         fromSrc: Option[String] = None,
                         repo: Option[String] = None,
                         tag: Option[String] = None,
                         message: Option[String] = None,
                         platform: Option[String] = None) extends MethodWithParams[List[model.ImagePullStatusEntry]]
  object ImageCreate {
    implicit val reader: JsonReader[ImageCreate] = jsonReader[ImageCreate]
    implicit val writer: JsonWriter[ImageCreate] = classWriter[ImageCreate] ++ jsonWriter[ImageCreate]
  }
  //#endregion
}
