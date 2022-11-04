package xyz.cofe.lima.docker.log

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.jackson._
import tethys.readers.{FieldName, ReaderError}
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.TokenWriter
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.model.{CreateContainerRequest, Image}
import xyz.cofe.lima.store.json._
import xyz.cofe.lima.store.json.Query._
import xyz.cofe.lima.store.json.TethysToks._
import xyz.cofe.lima.thread.ThreadID
import xyz.cofe.lima.errors.DockerClientError

import java.time.LocalDateTime

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
 * def containerProcesses(id:String): Either[String, model.Top] =
 *  <b>logger(ContainerProcesses(id))</b> // что за операция
 *    .run { // тут код операции
 *      sendForJson[model.Top](
 *        HttpRequest(path = s"/containers/${id}/top")
 *      )
 *    }
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

  def apply[M <: MethodCall,E](methodCall: M)(implicit jw:JsonWriter[M],jwe:JsonWriter[E]):ResultCall[E,methodCall.RESULT]
}

object Logger {
  trait ClientId {
    def clientId: Int
  }

  sealed trait MethodCall {
    type RESULT
    def parameters: (String, Product)
    def resultToJson(result: RESULT): String
  }

  sealed trait ResultCall[E, R] {
    def success(result: R)(implicit jw:JsonWriter[R]): R = result
    def error(error: E)(implicit jw:JsonWriter[E]): E = error
    def apply(code: => Either[E, R])(implicit clientId: ClientId, jw:JsonWriter[R], jw2:JsonWriter[E]): Either[E, R] = {
      code.left.map(error).map(success)
    }
    def run(code: => Either[E,R])(implicit clientId: ClientId, jw:JsonWriter[R], jw2:JsonWriter[E]): Either[E, R] = this.apply(code)
  }

  //#region Default logger

  case class DummyResult[E, R]() extends ResultCall[E, R]

  implicit val defaultLogger: Logger = new Logger {
    override def apply[M <: MethodCall,E](methodCall: M)(implicit jw:JsonWriter[M], jwe:JsonWriter[E]): ResultCall[E, methodCall.RESULT] = DummyResult()
  }

  //#endregion
  //#region stdout logger

//  def stdout:Logger = new Logger {
//    case class ResultCapture[R,M]( params:M ) extends ResultCall[String,R] {
//      override def success(result: R): R = {
//        println(s"DockerClient call ${params} succ ${result}")
//        result
//      }
//      override def error(error: String): String = {
//        println(s"DockerClient call ${params} error ${error}")
//        error
//      }
//      override def apply(code: => Either[String, R])(implicit clientId: ClientId): Either[String, R] = super.apply(code)
//    }
//    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = ResultCapture(methodCall)
//  }

  //#endregion
  //#region errorCapture logger

//  case class CapturedError(errorMessage:String, method:String, params:String)
//  def errorCapture( capturedError: CapturedError=>Unit ):Logger = new Logger {
//    case class ResultCapture[R,M <: MethodCall](methodWithParams:M) extends ResultCall[String,R] {
//      override def success(result: R): R = result
//      override def error(error: String): String = {
//
//        val (methodName,params) = methodWithParams.parameters
//
//        capturedError(
//          CapturedError(
//            errorMessage = error,
//            method=methodName,
//            params=params.productElementNames.zip(params.productIterator)
//              .map {case(name,value)=>s"$name=$value"}
//              .mkString("\n"),
//          )
//        )
//        error
//      }
//      override def apply(code: => Either[String, R])(implicit clientId: ClientId): Either[String, R] = super.apply(code)
//    }
//    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = ResultCapture(methodCall)
//  }

  //#endregion
  //#region joinLoggers

  def joinLoggers(left:Logger, right:Logger):Logger = new Logger {
    case class ResultCapture[M <: MethodCall:JsonWriter,R,E]( left:ResultCall[E,R], right:ResultCall[E,R] ) extends ResultCall[E,R] {
      override def success(result: R)(implicit jw: JsonWriter[R]): R = {
        left.success(result)
        right.success(result)
        super.success(result)
      }
      override def error(error: E)(implicit jw: JsonWriter[E]): E = {
        left.error(error)
        right.error(error)
        super.error(error)
      }
    }
    override def apply[M <: MethodCall,E](methodCall: M)(implicit jw: JsonWriter[M],jwe:JsonWriter[E]): ResultCall[E, methodCall.RESULT] = {
      val l: ResultCall[E, methodCall.RESULT] = left.apply[M,E](methodCall)
      val r: ResultCall[E, methodCall.RESULT] = right.apply[M,E](methodCall)
      ResultCapture(l,r)
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
    //implicit def reader[A:JsonReader,R:JsonReader]:JsonReader[FailEvent[A,R]] = jsonReader[FailEvent[A,R]]
  }

  /**
   * Запись событий в форме Json в поток
   * @param out поток куда записываются события
   */
  class JsonToWriter(out:java.lang.Appendable) extends Logger {
    case class ResultCapture[R,M <: Logger.MethodCall:JsonWriter,E]( beginCall: LocalDateTime, params:M ) extends ResultCall[E,R] {
      override def success(result: R)(implicit jw:JsonWriter[R]): R = {
        out.append(SuccEvent[M,R](
          ThreadID.current,
          beginCall,
          LocalDateTime.now(),
          params,
          result
        ).asJson).append(System.lineSeparator())
        super.success(result)
      }
      override def error(error: E)(implicit jw:JsonWriter[E]): E = {

        out.append(FailEvent(
          ThreadID.current,
          beginCall,
          LocalDateTime.now(),
          params,
          error
        ).asJson).append(System.lineSeparator())
        super.error(error)(jw)
      }
    }

    override def apply[M <: MethodCall,E](methodCall: M)(implicit jw: JsonWriter[M], jwe:JsonWriter[E]): ResultCall[E, methodCall.RESULT] =
      ResultCapture[methodCall.RESULT,M,E](LocalDateTime.now(),methodCall)
  }
  object JsonToWriter {
    def apply(out:java.lang.Appendable):JsonToWriter = new JsonToWriter(out)
  }

//  implicit def logEventJsonReader(implicit dtFormat:DateTimeFormatterProvide):JsonReader[LogEvent[_,_]] = {
//    val jsReader = implicitly[JsonReader[JsValue]]
//    val expectMethods = List(
//      "Containers","ContainerInspect","ContainerProcesses","ContainerLogs","ContainerStart","ContainerStop",
//      "ContainerCreate","ContainerKill","ContainerRemove","ContainerFsChanges",
//      "Images","ImageRemove","ImageTag","ImageHistory","ImageHistory","ImageInspect","ImageCreate"
//    )
//
//    new JsonReader[LogEvent[_,_]] {
//      override def read(it: TokenIterator)(implicit fieldName: FieldName): LogEvent[_,_] = {
//        val js = jsReader.read(it)
//        js.query("_type").string match {
//          case None => throw new Error("type of log event (_type = SuccEvent | FailEvent) not defined in root json")
//          case Some(logEventType) => logEventType match {
//            case "SuccEvent" | "FailEvent" => js.query("args")("_type").string match {
//              case None => throw new Error("type of method in event (args._type) not defined")
//              case Some(methodType) => methodType match {
//                case _ if expectMethods.contains(methodType) => restoreEvent(js, logEventType, methodType)
//                case _ => throw new Error(s"type of method $methodType incorrect, expect $expectMethods")
//              }
//            }
//            case _ => throw new Error(s"type of log incorrect $logEventType, expect SuccEvent | FailEvent")
//          }
//        }
//      }
//
//      private def restoreEvent(js:JsValue, logEventType:String, methodType:String):LogEvent[_,_] = {
//        js.query("args").jsObject match {
//          case None => throw new Error("field args not found")
//          case Some(args) => js.query("threadId").jsObject match {
//            case None => throw new Error("field threadId not found")
//            case Some(threadIdObj) => js.query("beginTime").jsValue match {
//              case None => throw new Error("field beginTime not found")
//              case Some(beginTime) => js.query("endTime").jsValue match {
//                case None => throw new Error("field endTime not found")
//                case Some(endTime) => logEventType match {
//                  case "SuccEvent" => js.query("result").jsValue match {
//                    case None => throw new Error("field result not found")
//                    case Some(result) => restoreSuccEvent(args, methodType, threadIdObj, beginTime, endTime, result)
//                  }
//                  case "FailEvent" => js.query("error").jsValue match {
//                    case None => throw new Error("field error not found")
//                    case Some(error) => restoreFailEvent(args, methodType, threadIdObj, beginTime, endTime, error)
//                  }
//                }
//              }
//            }
//          }
//        }
//      }
//
//      private def restoreSuccEvent(args:JsValue, methodType:String, threadIdObj:JsValue, beginTime:JsValue, endTime:JsValue, result:JsValue):LogEvent[_,_] = {
//        methodType match {
//          case "Containers" =>         restoreSucc[Containers]        (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerInspect" =>   restoreSucc[ContainerInspect]  (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerProcesses" => restoreSucc[ContainerProcesses](args,threadIdObj,beginTime,endTime,result)
//          case "ContainerLogs" =>      restoreSucc[ContainerLogs]     (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerStart" =>     restoreSucc[ContainerStart]    (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerStop" =>      restoreSucc[ContainerStop]     (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerCreate" =>    restoreSucc[ContainerCreate]   (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerKill" =>      restoreSucc[ContainerKill]     (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerRemove" =>    restoreSucc[ContainerRemove]   (args,threadIdObj,beginTime,endTime,result)
//          case "ContainerFsChanges" => restoreSucc[ContainerFsChanges](args,threadIdObj,beginTime,endTime,result)
//          case "Images" =>             restoreSucc[Images]            (args,threadIdObj,beginTime,endTime,result)
//          case "ImageRemove" =>        restoreSucc[ImageRemove]       (args,threadIdObj,beginTime,endTime,result)
//          case "ImageTag" =>           restoreSucc[ImageTag]          (args,threadIdObj,beginTime,endTime,result)
//          case "ImageHistory" =>       restoreSucc[ImageHistory]      (args,threadIdObj,beginTime,endTime,result)
//          case "ImageInspect" =>       restoreSucc[ImageInspect]      (args,threadIdObj,beginTime,endTime,result)
//          case "ImageCreate" =>        restoreSucc[ImageCreate]       (args,threadIdObj,beginTime,endTime,result)
//        }
//      }
//
//      private def restoreSucc[M<:MethodCall](
//                                              argsJs:JsValue,
//                                              threadIdObjJs:JsValue,
//                                              beginTimeJs:JsValue,
//                                              endTimeJs:JsValue,
//                                              resultJs:JsValue
//                                            )(implicit
//                                              methReader:JsonReader[M],
//                                              resultReader:JsonReader[M#RESULT],
//                                              methWriter:JsonWriter[M],
//                                              resultWriter:JsonWriter[M#RESULT]
//      )
//      :SuccEvent[M,M#RESULT] = {
//        (for {
//          margs <- argsJs.jsonAs[M]
//          mrest <- resultJs.jsonAs[M#RESULT]
//          thId <- threadIdObjJs.jsonAs[ThreadID]
//          begin <- beginTimeJs.jsonAs[LocalDateTime]
//          end <- endTimeJs.jsonAs[LocalDateTime]
//          event = SuccEvent(thId, begin, end, margs, mrest)
//        } yield event) match {
//          case Left(err) => throw err
//          case Right(value) => value
//        }
//      }
//
//      private def restoreFailEvent(args:JsValue, methodType:String, threadIdObj:JsValue, beginTime:JsValue, endTime:JsValue, error:JsValue):LogEvent[_,_] = {
//        methodType match {
//          case "Containers" =>         restoreFail[Containers]         (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerInspect" =>   restoreFail[ContainerInspect]   (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerProcesses" => restoreFail[ContainerProcesses] (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerLogs" =>      restoreFail[ContainerLogs]      (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerStart" =>     restoreFail[ContainerStart]     (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerStop" =>      restoreFail[ContainerStop]      (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerCreate" =>    restoreFail[ContainerCreate]    (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerKill" =>      restoreFail[ContainerKill]      (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerRemove" =>    restoreFail[ContainerRemove]    (threadIdObj, beginTime, endTime, args, error)
//          case "ContainerFsChanges" => restoreFail[ContainerFsChanges] (threadIdObj, beginTime, endTime, args, error)
//          case "Images" =>             restoreFail[Images]             (threadIdObj, beginTime, endTime, args, error)
//          case "ImageRemove" =>        restoreFail[ImageRemove]        (threadIdObj, beginTime, endTime, args, error)
//          case "ImageTag" =>           restoreFail[ImageTag]           (threadIdObj, beginTime, endTime, args, error)
//          case "ImageHistory" =>       restoreFail[ImageHistory]       (threadIdObj, beginTime, endTime, args, error)
//          case "ImageInspect" =>       restoreFail[ImageInspect]       (threadIdObj, beginTime, endTime, args, error)
//          case "ImageCreate" =>        restoreFail[ImageCreate]        (threadIdObj, beginTime, endTime, args, error)
//        }
//      }
//
//      private def restoreFail[M<:MethodCall](threadIdObj:JsValue, beginTime:JsValue, endTime:JsValue, args:JsValue, error:JsValue)
//                                            (implicit methReader:JsonReader[M], methWriter:JsonWriter[M], errReader:JsonReader[String], errWriter:JsonWriter[String])
//      :FailEvent[M,String] = {
//        (for {
//          thId <- threadIdObj.jsonAs[ThreadID]
//          begin <- beginTime.jsonAs[LocalDateTime]
//          end <- endTime.jsonAs[LocalDateTime]
//          margs <- args.jsonAs[M]
//          err <- error.jsonAs[String]
//          event = FailEvent(thId, begin, end, margs, err)
//        } yield event) match {
//          case Left(err) => throw err
//          case Right(value) => value
//        }
//      }
//    }
//  }

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

  implicit def unit2Json: JsonWriter[Unit] = (_: Unit, _: TokenWriter) => {}
  implicit def json2unit: JsonReader[Unit] = new JsonReader[Unit] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Unit = {
      ()
    }
  }

  //implicit def

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
