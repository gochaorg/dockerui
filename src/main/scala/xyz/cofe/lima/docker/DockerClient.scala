package xyz.cofe.lima.docker

import xyz.cofe.lima.docker.http.{HttpLogger, HttpRequest, HttpResponse, HttpResponseReader, SocketChannelSupplier, SocketLogger}

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.model.{CreateContainerRequest, CreateContainerResponse}

import java.net.UnixDomainSocketAddress

case class DockerClient( socketChannel: SocketChannel,
                         sourceTimeout:Long=1000L,
                         readTimeout:  Long=1000L*30L,
                         cpuThrottling:Long=1
                       )
                       (implicit
                        httpLogger: HttpLogger,
                        socketLogger: SocketLogger
                       )
{
  socketChannel.configureBlocking(false)
  private def send0(request:HttpRequest):Either[String,HttpResponse] = {
    val headerBlock =
      ((request.method + " " + request.path + " " + request.proto + "\n") +
       ("HOST: "+request.host+"\n")+
       ("User-Agent: "+request.`User-Agent`+"\n")+
       ("Accept: "+request.Accept+"\n")+
       "\n"
      ).getBytes(StandardCharsets.UTF_8)

    val sendBB = ByteBuffer.allocate(headerBlock.size + request.body.size)
    sendBB.put(headerBlock)

    val bodyArr = request.body.toArray
    sendBB.put(bodyArr)
    sendBB.flip()

    socketLogger.send(sendBB)
    socketChannel.write(sendBB)

    val socketChannelSupplier = SocketChannelSupplier(socketChannel)
    HttpResponseReader(
      socketChannelSupplier,
      sourceTimeout = sourceTimeout,
      readTimeout = readTimeout,
      cpuThrottling = cpuThrottling
    ).read
  }

  private def sendForText(
                                 request:HttpRequest,
                                 responseWrapper:HttpResponse=>HttpResponse = r=>r
                               ):Either[String,String] = {
    for {
      _ <- Right(httpLogger.send(request))
      response0 <- send0(request)
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

  private def sendForJson[A:JsonReader](
                          request:HttpRequest,
                          responseWrapper:HttpResponse=>HttpResponse = r=>r
                        ):Either[String,A] = {
    for {
      _ <- Right(httpLogger.send(request))
      response0 <- send0(request)
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

  def containers(all:Boolean=false, limit:Option[Int]=None, size:Boolean=false)
  : Either[String, List[model.ContainerStatus]] = {
    val q = Map("all"->all.toString) ++
      (limit.map(l=>Map("limit" -> l.toString)).getOrElse(Map())) ++
      (if(size)Map("size"->size.toString)else(Map()))

    sendForJson[List[model.ContainerStatus]](
      HttpRequest(path = "/containers/json").queryString(q)
    )
  }

  def containerInspect(id:String): Either[String, model.ContainerInspect] =
    sendForJson[model.ContainerInspect](
      HttpRequest(path = s"/containers/${id}/json")
    )

  def containerProcesses(id:String): Either[String, model.Top] =
    sendForJson[model.Top](
      HttpRequest(path = s"/containers/${id}/top")
    )

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
    send0(
      HttpRequest(path = s"/containers/${containerId}/start", method = "POST")
    ) match {
      case Left(errMessage) =>
        errMessage match {
          case "No response: first line not read" =>
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
    send0(
      HttpRequest(path = s"/containers/${containerId}/stop", method = "POST")
    ) match {
      case Left(errMessage) =>
        errMessage match {
          case "No response: first line not read" =>
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

  def killContainer(containerId:String) = {
    send0(HttpRequest(s"/containers/$containerId/kill").post()) match {
      case Left(errMessage) =>
        errMessage match {
          case "No response: first line not read" =>
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

object DockerClient {
  def unixSocket(path:String)(implicit
                              httpLogger: HttpLogger,
                              socketLogger: SocketLogger
  ):DockerClient =
    DockerClient(SocketChannel.open(UnixDomainSocketAddress.of(path)))
}
