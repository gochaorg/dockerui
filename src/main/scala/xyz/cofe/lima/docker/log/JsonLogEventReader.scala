package xyz.cofe.lima.docker.log

import tethys.derivation.auto.jsonReaderMaterializer
import tethys.{JsonReader, JsonWriter}
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator
import xyz.cofe.lima.docker.errors._
import xyz.cofe.lima.docker.log.Logger._
import xyz.cofe.lima.store.json._
import xyz.cofe.lima.store.json.Query._
import xyz.cofe.lima.store.json.TethysToks._
import xyz.cofe.lima.thread.ThreadID

import java.time.LocalDateTime

/**
 * Чтение события [[LogEvent]] из Json
 */
object JsonLogEventReader {
  private val expectMethods = List(
    "Containers", "ContainerInspect", "ContainerProcesses", "ContainerLogs", "ContainerStart", "ContainerStop",
    "ContainerCreate", "ContainerKill", "ContainerRemove", "ContainerFsChanges",
    "Images", "ImageRemove", "ImageTag", "ImageHistory", "ImageHistory", "ImageInspect", "ImageCreate", "ImageSearch"
  )

  def reader(implicit dtFormat:DateTimeFormatterProvide):JsonReader[LogEvent[_,_]] = {
    val jsReader = implicitly[JsonReader[JsValue]]

    new JsonReader[LogEvent[_,_]] {
      override def read(it: TokenIterator)(implicit fieldName: FieldName): LogEvent[_,_] = {
        val js = jsReader.read(it)
        js.query("_type").string match {
          case None => throw new Error("type of log event (_type = SuccEvent | FailEvent) not defined in root json")
          case Some(logEventType) => logEventType match {
            case "SuccEvent" | "FailEvent" => js.query("args")("_type").string match {
              case None => throw new Error("type of method in event (args._type) not defined")
              case Some(methodType) => methodType match {
                case _ if expectMethods.contains(methodType) => restoreEvent(js, logEventType, methodType)
                case _ => throw new Error(s"type of method $methodType incorrect, expect $expectMethods")
              }
            }
            case _ => throw new Error(s"type of log incorrect $logEventType, expect SuccEvent | FailEvent")
          }
        }
      }

      private def restoreEvent(js:JsValue, logEventType:String, methodType:String):LogEvent[_,_] = {
        js.query("args").jsObject match {
          case None => throw new Error("field args not found")
          case Some(args) => js.query("threadId").jsObject match {
            case None => throw new Error("field threadId not found")
            case Some(threadIdObj) => js.query("beginTime").jsValue match {
              case None => throw new Error("field beginTime not found")
              case Some(beginTime) => js.query("endTime").jsValue match {
                case None => throw new Error("field endTime not found")
                case Some(endTime) => logEventType match {
                  case "SuccEvent" => js.query("result").jsValue match {
                    case None => throw new Error("field result not found")
                    case Some(result) => restoreSuccEvent(args, methodType, threadIdObj, beginTime, endTime, result)
                  }
                  case "FailEvent" => js.query("error").jsValue match {
                    case None => throw new Error("field error not found")
                    case Some(error) => restoreFailEvent(args, methodType, threadIdObj, beginTime, endTime, error)
                  }
                }
              }
            }
          }
        }
      }

      private def restoreSuccEvent(args:JsValue, methodType:String, threadIdObj:JsValue, beginTime:JsValue, endTime:JsValue, result:JsValue):LogEvent[_,_] = {
        methodType match {
          case "Containers" =>         restoreSucc[Containers]        (args,threadIdObj,beginTime,endTime,result)
          case "ContainerInspect" =>   restoreSucc[ContainerInspect]  (args,threadIdObj,beginTime,endTime,result)
          case "ContainerProcesses" => restoreSucc[ContainerProcesses](args,threadIdObj,beginTime,endTime,result)
          case "ContainerLogs" =>      restoreSucc[ContainerLogs]     (args,threadIdObj,beginTime,endTime,result)
          case "ContainerStart" =>     restoreSucc[ContainerStart]    (args,threadIdObj,beginTime,endTime,result)
          case "ContainerStop" =>      restoreSucc[ContainerStop]     (args,threadIdObj,beginTime,endTime,result)
          case "ContainerCreate" =>    restoreSucc[ContainerCreate]   (args,threadIdObj,beginTime,endTime,result)
          case "ContainerKill" =>      restoreSucc[ContainerKill]     (args,threadIdObj,beginTime,endTime,result)
          case "ContainerRemove" =>    restoreSucc[ContainerRemove]   (args,threadIdObj,beginTime,endTime,result)
          case "ContainerFsChanges" => restoreSucc[ContainerFsChanges](args,threadIdObj,beginTime,endTime,result)
          case "Images" =>             restoreSucc[Images]            (args,threadIdObj,beginTime,endTime,result)
          case "ImageRemove" =>        restoreSucc[ImageRemove]       (args,threadIdObj,beginTime,endTime,result)
          case "ImageTag" =>           restoreSucc[ImageTag]          (args,threadIdObj,beginTime,endTime,result)
          case "ImageHistory" =>       restoreSucc[ImageHistory]      (args,threadIdObj,beginTime,endTime,result)
          case "ImageInspect" =>       restoreSucc[ImageInspect]      (args,threadIdObj,beginTime,endTime,result)
          case "ImageCreate" =>        restoreSucc[ImageCreate]       (args,threadIdObj,beginTime,endTime,result)
          case "ImageSearch" =>        restoreSucc[ImageSearch]       (args,threadIdObj,beginTime,endTime,result)
        }
      }

      private def restoreSucc[M<:MethodCall](
                                              argsJs:JsValue,
                                              threadIdObjJs:JsValue,
                                              beginTimeJs:JsValue,
                                              endTimeJs:JsValue,
                                              resultJs:JsValue
                                            )(implicit
                                              methReader:JsonReader[M],
                                              resultReader:JsonReader[M#RESULT],
                                              methWriter:JsonWriter[M],
                                              resultWriter:JsonWriter[M#RESULT]
      )
      :SuccEvent[M,M#RESULT] = {
        (for {
          margs <- argsJs.jsonAs[M]
          mrest <- resultJs.jsonAs[M#RESULT]
          thId <- threadIdObjJs.jsonAs[ThreadID]
          begin <- beginTimeJs.jsonAs[LocalDateTime]
          end <- endTimeJs.jsonAs[LocalDateTime]
          event = SuccEvent(thId, begin, end, margs, mrest)
        } yield event) match {
          case Left(err) => throw err
          case Right(value) => value
        }
      }

      private def restoreFailEvent(args: JsValue, methodType: String, threadIdObj: JsValue, beginTime: JsValue, endTime: JsValue, error: JsValue): LogEvent[_, _] = {
        methodType match {
          case "Containers" => restoreFail[Containers](threadIdObj, beginTime, endTime, args, error)
          case "ContainerInspect" => restoreFail[ContainerInspect](threadIdObj, beginTime, endTime, args, error)
          case "ContainerProcesses" => restoreFail[ContainerProcesses](threadIdObj, beginTime, endTime, args, error)
          case "ContainerLogs" => restoreFail[ContainerLogs](threadIdObj, beginTime, endTime, args, error)
          case "ContainerStart" => restoreFail[ContainerStart](threadIdObj, beginTime, endTime, args, error)
          case "ContainerStop" => restoreFail[ContainerStop](threadIdObj, beginTime, endTime, args, error)
          case "ContainerCreate" => restoreFail[ContainerCreate](threadIdObj, beginTime, endTime, args, error)
          case "ContainerKill" => restoreFail[ContainerKill](threadIdObj, beginTime, endTime, args, error)
          case "ContainerRemove" => restoreFail[ContainerRemove](threadIdObj, beginTime, endTime, args, error)
          case "ContainerFsChanges" => restoreFail[ContainerFsChanges](threadIdObj, beginTime, endTime, args, error)
          case "Images" => restoreFail[Images](threadIdObj, beginTime, endTime, args, error)
          case "ImageRemove" => restoreFail[ImageRemove](threadIdObj, beginTime, endTime, args, error)
          case "ImageTag" => restoreFail[ImageTag](threadIdObj, beginTime, endTime, args, error)
          case "ImageHistory" => restoreFail[ImageHistory](threadIdObj, beginTime, endTime, args, error)
          case "ImageInspect" => restoreFail[ImageInspect](threadIdObj, beginTime, endTime, args, error)
          case "ImageCreate" => restoreFail[ImageCreate](threadIdObj, beginTime, endTime, args, error)
          case "ImageSearch" => restoreFail[ImageSearch](threadIdObj, beginTime, endTime, args, error)
        }
      }

      private def restoreFail[M <: MethodCall](threadIdObj: JsValue, beginTime: JsValue, endTime: JsValue, args: JsValue, error: JsValue)
                                              (implicit
                                               methReader: JsonReader[M], methWriter: JsonWriter[M],
                                               errReader: JsonReader[DockerError], errWriter: JsonWriter[DockerError])
      : FailEvent[M, DockerError] = {
        (for {
          thId <- threadIdObj.jsonAs[ThreadID]
          begin <- beginTime.jsonAs[LocalDateTime]
          end <- endTime.jsonAs[LocalDateTime]
          margs <- args.jsonAs[M]
          err <- error.jsonAs[DockerError]
          event = FailEvent(thId, begin, end, margs, err)
        } yield event) match {
          case Left(err) => throw err
          case Right(value) => value
        }
      }
    }
  }
}
