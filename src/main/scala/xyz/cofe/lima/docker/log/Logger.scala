package xyz.cofe.lima.docker.log

import tethys.JsonWriter
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.model.{CreateContainerRequest, Image}

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
    case class ResultCapture[R,M <: MethodCall](methodWithParams:M) extends ResultCall[String,R] {
      override def success(result: R): R = result
      override def error(error: String): String = {

        val (methodName,params) = methodWithParams.parameters

        capturedError(
          CapturedError(
            errorMessage = error,
            method=methodName,
            params=params.productElementNames.zip(params.productIterator)
              .map {case(name,value)=>s"$name=$value"}
              .mkString("\n"),
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
    def parameters:(String,Product)
    def resultToJson(result:RESULT):String
  }

  sealed trait ResultCall[E,R] {
    def success(result:R):R=result
    def error(error:E):E=error
    def apply(code: =>Either[E,R])(implicit clientId: ClientId): Either[E, R] ={
      code.left.map(error).map(success)
    }
  }
  case class DummyResult[E,R]() extends ResultCall[E,R]

  //#region Method and result

  abstract class MethodWithParams[A:JsonWriter] extends Product with MethodCall {
    import tethys._
    import tethys.jackson._
    override type RESULT = A
    override def parameters: (String, Product) = (this.getClass.getSimpleName,this)
    override def resultToJson(result:RESULT):String = result.asJson
  }

  case class Containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false) extends MethodWithParams[List[model.ContainerStatus]]
  case class ContainerInspect(id:String) extends MethodWithParams[model.ContainerInspect]
  case class ContainerProcesses(id:String) extends MethodWithParams[model.Top]

  implicit def arrayOfString2Json:JsonWriter[Array[String]] = (value: Array[String], tokenWriter: TokenWriter) => {
    tokenWriter.writeArrayStart()
    value.foreach(itm => {
      tokenWriter.writeString(itm)
    })
    tokenWriter.writeArrayEnd()
  }
  implicit def unit2Json:JsonWriter[Unit] = (_: Unit, _: TokenWriter) => {}

  case class ContainerLogs(id:String,
                           follow:Option[Boolean]=None,
                           stdout:Option[Boolean]=Some(true),
                           stderr:Option[Boolean]=None,
                           since:Option[Long]=None,
                           timestamps:Option[Boolean]=Some(true),
                           tail:Option[String]=None) extends MethodWithParams[Array[String]]
  case class ContainerStart(containerId:String) extends MethodWithParams[Unit]
  case class ContainerStop(containerId:String) extends MethodWithParams[Unit]
  case class ContainerCreate(
                              createContainerRequest: CreateContainerRequest,
                              name:Option[String]=None,
                              platform:Option[String]=None
                            )extends MethodWithParams[model.CreateContainerResponse]
  case class ContainerKill(containerId:String) extends MethodWithParams[Unit]
  case class ContainerRemove(containerId:String,
                             anonVolumesRemove:Option[Boolean]=None,
                             force:Option[Boolean]=None,
                             link:Option[Boolean]=None ) extends MethodWithParams[Unit]
  case class ContainerFsChanges(containerId:String) extends MethodWithParams[List[model.ContainerFileChanges]]
  case class ContainerRename(containerId:String, newName:String) extends MethodWithParams[Unit]
  case class Images() extends MethodWithParams[List[Image]]
  case class ImageRemove(nameOrId:String,
                         force:Option[Boolean]=None,
                         noprune:Option[Boolean]=None) extends MethodWithParams[List[model.ImageRemove]]
  case class ImageTag(nameOrId:String,
                      repo:Option[String]=None,
                      tag:Option[String]=None) extends MethodWithParams[Unit]
  case class ImageHistory(nameOrId:String) extends MethodWithParams[List[model.ImageHistory]]
  case class ImageInspect(nameOrId:String) extends MethodWithParams[model.ImageInspect]
  //#endregion
}
