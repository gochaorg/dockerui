package xyz.cofe.lima.docker.log

import tethys.JsonWriter
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.model.{CreateContainerRequest, CreateContainerResponse, Image}

trait Logger {
  import Logger._

  def apply[M <: MethodCall](methodCall: M):ResultCall[String,methodCall.RESULT]
}

object Logger {
  implicit val defaultLogger: Logger = new Logger {
    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = DummyResult()
  }
  def stdout:Logger = new Logger {
    case class ResultCapture[R,M]( params:M ) extends ResultCall[String,R] {
      override def success(result: R): R = {
        println(s"DockerClient call ${params} succ ${result}")
        result
      }
      override def error(error: String): String = {
        println(s"DockerClient call ${params} error ${error}")
        error
      }
      override def apply(code: => Either[String, R])(implicit clientId: ClientId): Either[String, R] = super.apply(code)
    }
    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = ResultCapture(methodCall)
  }

  case class CapturedError(errorMessage:String, method:String, params:String)
  def errorCapture( capturedError: CapturedError=>Unit ):Logger = new Logger {
    case class ResultCapture[R,M <: MethodCall]( params:M ) extends ResultCall[String,R] {
      override def success(result: R): R = result
      override def error(error: String): String = {
        import tethys._
        import tethys.jackson._
        capturedError(
          CapturedError(
            errorMessage = error,
            params=(params:MethodCall).asJson,
            method=params.getClass.getSimpleName
          )
        )
        error
      }
      override def apply(code: => Either[String, R])(implicit clientId: ClientId): Either[String, R] = super.apply(code)
    }
    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = ResultCapture(methodCall)
  }

  def joinLoggers(left:Logger, right:Logger):Logger = new Logger {
    case class ResultCapture[R,M <: MethodCall]( left:ResultCall[String,R], right:ResultCall[String,R] ) extends ResultCall[String,R] {
      override def apply(code: => Either[String, R])(implicit clientId: ClientId): Either[String, R] = {
        val l = left(code)
        val r = right(code)
        l
      }
    }
    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = {
      ResultCapture(
        left = left(methodCall),
        right = right(methodCall)
      )
    }
  }

  trait ClientId {
    def clientId:Int
  }

  sealed trait MethodCall {
    type RESULT
  }

  implicit def methodCallJsonWriter: JsonWriter[MethodCall] = new JsonWriter[MethodCall] {
    override def write(value: MethodCall, tokenWriter: TokenWriter): Unit = {
      value match {
        case c:Containers => Containers.writer.write(c,tokenWriter)
        case c:ContainerInspect => ContainerInspect.writer.write(c,tokenWriter)
        case c:ContainerProcesses => ContainerProcesses.writer.write(c,tokenWriter)
        case c:ContainerLogs => ContainerLogs.writer.write(c,tokenWriter)
        case c:ContainerStart => ContainerStart.writer.write(c,tokenWriter)
        case c:ContainerStop => ContainerStop.writer.write(c,tokenWriter)
        case c:ContainerCreate => ContainerCreate.writer.write(c,tokenWriter)
        case c:ContainerKill => ContainerKill.writer.write(c,tokenWriter)
        case c:ContainerRemove => ContainerRemove.writer.write(c,tokenWriter)
        case c:ContainerFsChanges => ContainerFsChanges.writer.write(c,tokenWriter)
        case c:ContainerRename => ContainerRename.writer.write(c,tokenWriter)
        case c:Images => Images.writer.write(c,tokenWriter)
        case c:ImageRemove => ImageRemove.writer.write(c,tokenWriter)
        case c:ImageTag => ImageTag.writer.write(c,tokenWriter)
        case c:ImageHistory => ImageHistory.writer.write(c,tokenWriter)
        case c:ImageInspect => ImageInspect.writer.write(c,tokenWriter)
      }
    }
  }

  sealed trait ResultCall[E,R] {
    def success(result:R):R=result
    def error(error:E):E=error
    def apply(code: =>Either[E,R])(implicit clientId: ClientId): Either[E, R] ={
      code.left.map(error).map(success)
    }
  }
  case class DummyResult[E,R]() extends ResultCall[E,R] {
  }

  //#region Method and result

  case class Containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false) extends MethodCall {
    override type RESULT = List[model.ContainerStatus]
  }
  object Containers {
    implicit val reader = jsonReader[Containers]
    implicit val writer = jsonWriter[Containers]
  }

  case class ContainerInspect(id:String) extends MethodCall {
    override type RESULT = model.ContainerInspect
  }
  object ContainerInspect {
    implicit val reader = jsonReader[ContainerInspect]
    implicit val writer = jsonWriter[ContainerInspect]
  }

  case class ContainerProcesses(id:String) extends MethodCall {
    override type RESULT = model.Top
  }
  object ContainerProcesses {
    implicit val reader = jsonReader[ContainerProcesses]
    implicit val writer = jsonWriter[ContainerProcesses]
  }

  case class ContainerLogs(id:String,
                           follow:Option[Boolean]=None,
                           stdout:Option[Boolean]=Some(true),
                           stderr:Option[Boolean]=None,
                           since:Option[Long]=None,
                           timestamps:Option[Boolean]=Some(true),
                           tail:Option[String]=None) extends MethodCall{
    override type RESULT = Array[String]
  }
  object ContainerLogs {
    implicit val reader = jsonReader[ContainerLogs]
    implicit val writer = jsonWriter[ContainerLogs]
  }

  case class ContainerStart(containerId:String) extends MethodCall {
    override type RESULT = Unit
  }
  object ContainerStart {
    implicit val reader = jsonReader[ContainerStart]
    implicit val writer = jsonWriter[ContainerStart]
  }

  case class ContainerStop(containerId:String) extends MethodCall {
    override type RESULT = Unit
  }
  object ContainerStop {
    implicit val reader = jsonReader[ContainerStop]
    implicit val writer = jsonWriter[ContainerStop]
  }

  case class ContainerCreate(
                              createContainerRequest: CreateContainerRequest,
                              name:Option[String]=None,
                              platform:Option[String]=None
                            )extends MethodCall {
    override type RESULT = CreateContainerResponse
  }
  object ContainerCreate {
    implicit val reader = jsonReader[ContainerCreate]
    implicit val writer = jsonWriter[ContainerCreate]
  }

  case class ContainerKill(containerId:String) extends MethodCall {
    override type RESULT = Unit
  }
  object ContainerKill {
    implicit val reader = jsonReader[ContainerKill]
    implicit val writer = jsonWriter[ContainerKill]
  }

  case class ContainerRemove(containerId:String, anonVolumesRemove:Option[Boolean]=None, force:Option[Boolean]=None, link:Option[Boolean]=None ) extends MethodCall {
    override type RESULT = Unit
  }
  object ContainerRemove {
    implicit val reader = jsonReader[ContainerRemove]
    implicit val writer = jsonWriter[ContainerRemove]
  }

  case class ContainerFsChanges(containerId:String) extends MethodCall {
    override type RESULT = List[model.ContainerFileChanges]
  }
  object ContainerFsChanges {
    implicit val reader = jsonReader[ContainerFsChanges]
    implicit val writer = jsonWriter[ContainerFsChanges]
  }

  case class ContainerRename(containerId:String, newName:String) extends MethodCall {
    override type RESULT = Unit
  }
  object ContainerRename {
    implicit val reader = jsonReader[ContainerRename]
    implicit val writer = jsonWriter[ContainerRename]
  }

  case class Images() extends MethodCall {
    override type RESULT = List[Image]
  }
  object Images {
    implicit val reader = jsonReader[Images]
    implicit val writer = jsonWriter[Images]
  }

  case class ImageRemove(nameOrId:String,force:Option[Boolean]=None,noprune:Option[Boolean]=None) extends MethodCall {
    override type RESULT = List[model.ImageRemove]
  }
  object ImageRemove {
    implicit val reader = jsonReader[ImageRemove]
    implicit val writer = jsonWriter[ImageRemove]
  }

  case class ImageTag(nameOrId:String,repo:Option[String]=None,tag:Option[String]=None) extends MethodCall {
    override type RESULT = Unit
  }
  object ImageTag {
    implicit val reader = jsonReader[ImageTag]
    implicit val writer = jsonWriter[ImageTag]
  }

  case class ImageHistory(nameOrId:String) extends MethodCall {
    override type RESULT = List[model.ImageHistory]
  }
  object ImageHistory {
    implicit val reader = jsonReader[ImageHistory]
    implicit val writer = jsonWriter[ImageHistory]
  }

  case class ImageInspect(nameOrId:String) extends MethodCall {
    override type RESULT = model.ImageInspect
  }
  object ImageInspect {
    implicit val reader = jsonReader[ImageInspect]
    implicit val writer = jsonWriter[ImageInspect]
  }
  //#endregion
}
