package xyz.cofe.lima.docker.log

import tethys.JsonObjectWriter.lowPriorityWriter
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import tethys.{JsonReader, JsonWriter}
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.model.{CreateContainerRequest, Image}
import xyz.cofe.lima.store.json._

import java.time.Instant

trait Logger {
  import Logger._

  def apply[M <: MethodCall](methodCall: M)(implicit jw:JsonWriter[M]):ResultCall[String,methodCall.RESULT]
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
    override def apply[M <: MethodCall](methodCall: M)(implicit jw:JsonWriter[M]): ResultCall[String, methodCall.RESULT] = DummyResult()
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
    case class ResultCapture[M <: MethodCall:JsonWriter,R]( left:ResultCall[String,R], right:ResultCall[String,R] ) extends ResultCall[String,R] {
      override def success(result: R)(implicit jw: JsonWriter[R]): R = {
        left.success(result)
        right.success(result)
        super.success(result)
      }
      override def error(error: String)(implicit jw: JsonWriter[String]): String = {
        left.error(error)
        right.error(error)
        super.error(error)
      }
    }
    override def apply[M <: MethodCall](methodCall: M)(implicit jw: JsonWriter[M]): ResultCall[String, methodCall.RESULT] = {
      val l: ResultCall[String, methodCall.RESULT] = left.apply[M](methodCall)
      val r: ResultCall[String, methodCall.RESULT] = right.apply[M](methodCall)
      ResultCapture(l,r)
    }
  }
  //#endregion

  sealed trait LogEvent[ARGS] {
    def args:ARGS
  }
  case class SuccEvent[ARGS:JsonWriter,RES:JsonWriter](args:ARGS, result:RES) extends LogEvent[ARGS]
  object SuccEvent {
    implicit def writer[A:JsonWriter,R:JsonWriter]:JsonWriter[SuccEvent[A,R]] = jsonWriter[SuccEvent[A,R]]
  }

  case class FailEvent[ARGS:JsonWriter,ERR:JsonWriter](args:ARGS, error:ERR) extends LogEvent[ARGS]
  object FailEvent {
    implicit def writer[A:JsonWriter,R:JsonWriter]:JsonWriter[FailEvent[A,R]] = jsonWriter[FailEvent[A,R]]
  }

  class JsonToWriter(out:java.lang.Appendable) extends Logger {
    case class ResultCapture[R,M <: Logger.MethodCall:JsonWriter]( params:M ) extends ResultCall[String,R] {
      override def success(result: R)(implicit jw:JsonWriter[R]): R = {
        out.append(SuccEvent[M,R](params,result).asJson).append(System.lineSeparator())
        super.success(result)
      }
      override def error(error: String)(implicit jw:JsonWriter[String]): String = {
        out.append(FailEvent(params,error).asJson).append(System.lineSeparator())
        super.error(error)
      }
    }

    override def apply[M <: MethodCall](methodCall: M)(implicit jw: JsonWriter[M]): ResultCall[String, methodCall.RESULT] =
      ResultCapture[methodCall.RESULT,M](methodCall)
  }

  //#region log events

  implicit def arrayOfString2Json: JsonWriter[Array[String]] = (value: Array[String], tokenWriter: TokenWriter) => {
    tokenWriter.writeArrayStart()
    value.foreach(itm => {
      tokenWriter.writeString(itm)
    })
    tokenWriter.writeArrayEnd()
  }
  implicit def unit2Json: JsonWriter[Unit] = (_: Unit, _: TokenWriter) => {}

  abstract class MethodWithParams[A:JsonWriter] extends Product with MethodCall {
    import tethys._
    import tethys.jackson._
    override type RESULT = A
    override def parameters: (String, Product) = (this.getClass.getSimpleName,this)
    override def resultToJson(result:RESULT):String = result.asJson
  }

  case class Containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false) extends MethodWithParams[List[model.ContainerStatus]]
  object Containers {
    implicit val reader: JsonReader[Containers] = jsonReader[Containers]
    implicit val writer: JsonWriter[Containers] = classWriter[Containers] ++ jsonWriter[Containers]
  }

  case class ContainerInspect(id:String) extends MethodWithParams[model.ContainerInspect]
  object ContainerInspect {
    implicit val reader: JsonReader[ContainerInspect] = jsonReader[ContainerInspect]
    implicit val writer: JsonWriter[ContainerInspect] = classWriter[ContainerInspect] ++ jsonWriter[ContainerInspect]
  }

  case class ContainerProcesses(id:String) extends MethodWithParams[model.Top]
  object ContainerProcesses {
    implicit val reader: JsonReader[ContainerProcesses] = jsonReader[ContainerProcesses]
    implicit val writer: JsonWriter[ContainerProcesses] = classWriter[ContainerProcesses] ++ jsonWriter[ContainerProcesses]
  }

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

  case class ContainerStart(containerId:String) extends MethodWithParams[Unit]
  object ContainerStart {
    implicit val reader: JsonReader[ContainerStart] = jsonReader[ContainerStart]
    implicit val writer: JsonWriter[ContainerStart] = classWriter[ContainerStart] ++ jsonWriter[ContainerStart]
  }

  case class ContainerStop(containerId:String) extends MethodWithParams[Unit]
  object ContainerStop {
    implicit val reader: JsonReader[ContainerStop] = jsonReader[ContainerStop]
    implicit val writer: JsonWriter[ContainerStop] = classWriter[ContainerStop] ++ jsonWriter[ContainerStop]
  }

  case class ContainerCreate(
                              createContainerRequest: CreateContainerRequest,
                              name:Option[String]=None,
                              platform:Option[String]=None
                            ) extends MethodWithParams[model.CreateContainerResponse]
  object ContainerCreate {
    implicit val reader: JsonReader[ContainerCreate] = jsonReader[ContainerCreate]
    implicit val writer: JsonWriter[ContainerCreate] = classWriter[ContainerCreate] ++ jsonWriter[ContainerCreate]
  }

  case class ContainerKill(containerId:String) extends MethodWithParams[Unit]
  object ContainerKill {
    implicit val reader: JsonReader[ContainerKill] = jsonReader[ContainerKill]
    implicit val writer: JsonWriter[ContainerKill] = classWriter[ContainerKill] ++ jsonWriter[ContainerKill]
  }

  case class ContainerRemove(containerId:String,
                             anonVolumesRemove:Option[Boolean]=None,
                             force:Option[Boolean]=None,
                             link:Option[Boolean]=None ) extends MethodWithParams[Unit]
  object ContainerRemove {
    implicit val reader: JsonReader[ContainerRemove] = jsonReader[ContainerRemove]
    implicit val writer: JsonWriter[ContainerRemove] = classWriter[ContainerRemove] ++ jsonWriter[ContainerRemove]
  }

  case class ContainerFsChanges(containerId:String) extends MethodWithParams[List[model.ContainerFileChanges]]
  object ContainerFsChanges {
    implicit val reader: JsonReader[ContainerFsChanges] = jsonReader[ContainerFsChanges]
    implicit val writer: JsonWriter[ContainerFsChanges] = classWriter[ContainerFsChanges] ++ jsonWriter[ContainerFsChanges]
  }

  case class ContainerRename(containerId:String, newName:String) extends MethodWithParams[Unit]
  object ContainerRename {
    implicit val reader: JsonReader[ContainerRename] = jsonReader[ContainerRename]
    implicit val writer: JsonWriter[ContainerRename] = classWriter[ContainerRename] ++ jsonWriter[ContainerRename]
  }

  case class Images() extends MethodWithParams[List[Image]]
  object Images {
    implicit val reader: JsonReader[Images] = jsonReader[Images]
    implicit val writer: JsonWriter[Images] = classWriter[Images] ++ jsonWriter[Images]
  }

  case class ImageRemove(nameOrId:String,
                         force:Option[Boolean]=None,
                         noprune:Option[Boolean]=None) extends MethodWithParams[List[model.ImageRemove]]
  object ImageRemove {
    implicit val reader: JsonReader[ImageRemove] = jsonReader[ImageRemove]
    implicit val writer: JsonWriter[ImageRemove] = classWriter[ImageRemove] ++ jsonWriter[ImageRemove]
  }

  case class ImageTag(nameOrId:String,
                      repo:Option[String]=None,
                      tag:Option[String]=None) extends MethodWithParams[Unit]
  object ImageTag {
    implicit val reader: JsonReader[ImageTag] = jsonReader[ImageTag]
    implicit val writer: JsonWriter[ImageTag] = classWriter[ImageTag] ++ jsonWriter[ImageTag]
  }

  case class ImageHistory(nameOrId:String) extends MethodWithParams[List[model.ImageHistory]]
  object ImageHistory {
    implicit val reader: JsonReader[ImageHistory] = jsonReader[ImageHistory]
    implicit val writer: JsonWriter[ImageHistory] = classWriter[ImageHistory] ++ jsonWriter[ImageHistory]
  }

  case class ImageInspect(nameOrId:String) extends MethodWithParams[model.ImageInspect]
  object ImageInspect {
    implicit val reader: JsonReader[ImageInspect] = jsonReader[ImageInspect]
    implicit val writer: JsonWriter[ImageInspect] = classWriter[ImageInspect] ++ jsonWriter[ImageInspect]
  }

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
