package xyz.cofe.lima.docker.log

import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.model.{CreateContainerRequest, CreateContainerResponse, Image}

trait Logger {
  import Logger._

  def apply[M <: MethodCall](methodCall: M):ResultCall[String,methodCall.RESULT]
}

object Logger {
  trait ClientId {
    def clientId:Int
  }

  sealed trait MethodCall {
    type RESULT
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

  implicit val defaultLogger: Logger = new Logger {
    override def apply[M <: MethodCall](methodCall: M): ResultCall[String, methodCall.RESULT] = DummyResult()
  }

  case class Containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false) extends MethodCall {
    override type RESULT = List[model.ContainerStatus]
  }
  case class ContainerInspect(id:String) extends MethodCall {
    override type RESULT = model.ContainerInspect
  }
  case class ContainerProcesses(id:String) extends MethodCall {
    override type RESULT = model.Top
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
  case class ContainerStart(containerId:String) extends MethodCall {
    override type RESULT = Unit
  }
  case class ContainerStop(containerId:String) extends MethodCall {
    override type RESULT = Unit
  }
  case class ContainerCreate(
                              createContainerRequest: CreateContainerRequest,
                              name:Option[String]=None,
                              platform:Option[String]=None
                            )extends MethodCall {
    override type RESULT = CreateContainerResponse
  }
  case class ContainerKill(containerId:String) extends MethodCall {
    override type RESULT = Unit
  }
  case class ContainerRemove(containerId:String, anonVolumesRemove:Option[Boolean]=None, force:Option[Boolean]=None, link:Option[Boolean]=None ) extends MethodCall {
    override type RESULT = Unit
  }
  case class ContainerFsChanges(containerId:String) extends MethodCall {
    override type RESULT = List[model.ContainerFileChanges]
  }
  case class ContainerRename(containerId:String, newName:String) extends MethodCall {
    override type RESULT = Unit
  }
  case class Images() extends MethodCall {
    override type RESULT = List[Image]
  }
  case class ImageRemove(nameOrId:String,force:Option[Boolean]=None,noprune:Option[Boolean]=None) extends MethodCall {
    override type RESULT = List[model.ImageRemove]
  }
  case class ImageTag(nameOrId:String,repo:Option[String]=None,tag:Option[String]=None) extends MethodCall {
    override type RESULT = Unit
  }
  case class ImageHistory(nameOrId:String) extends MethodCall {
    override type RESULT = List[model.ImageHistory]
  }
  case class ImageInspect(nameOrId:String) extends MethodCall {
    override type RESULT = model.ImageInspect
  }
}
