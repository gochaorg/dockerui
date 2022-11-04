package xyz.cofe.lima.docker.http

import tethys.JsonReader
import xyz.cofe.lima.docker.http.HttpResponseStream.Event

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.util.concurrent.locks.{Lock, ReentrantLock}
import scala.reflect.ClassTag
import tethys._
import tethys.jackson._

trait HttpClient {
  type ERROR

  def stream(request:HttpRequest)(consumer:HttpResponseStream.Event=>HttpResponseStream.Behavior):Unit

  def http(request:HttpRequest):Either[ERROR,HttpResponse]

  def text(
      request: HttpRequest,
      responseWrapper: HttpResponse => HttpResponse = r => r,
      successHttpCode: HttpResponse => Boolean = r => r.isOk
    ): Either[ERROR, String]

  def json[A: JsonReader](
      request: HttpRequest,
      responseWrapper: HttpResponse => HttpResponse = r => r,
      successHttpCode: HttpResponse => Boolean = r => r.isOk
    )(implicit ct: ClassTag[A]): Either[ERROR, A]
}

object HttpClient {
  def apply(
             socketChannel: SocketChannel,
             socketReadTimings: SocketReadTimings = SocketReadTimings(),
             socketLock: Lock = new ReentrantLock()
           )(implicit socketLogger: SocketLogger, httpLogger: HttpLogger):HttpClientImpl = {
    new HttpClientImpl(socketChannel, socketLock, socketReadTimings)
  }
}

class HttpClientImpl
  (
    socketChannel: SocketChannel,
    socketLock: Lock = new ReentrantLock(),
    socketReadTimings: SocketReadTimings = SocketReadTimings()
  )(implicit
    val socketLogger: SocketLogger,
    val httpLogger: HttpLogger
  )
  extends HttpClient
{
  override type ERROR = errors.HttpError
  socketChannel.configureBlocking(false)

  def lockAndRun[R](code: => R): R = {
    try {
      socketLock.lock()
      code
    } finally {
      socketLock.unlock()
    }
  }
  def tryLockAndRun[R](code: => R): Option[R] = {
    var succLock = false
    try {
      if (socketLock.tryLock()) {
        succLock = true
        Some(code)
      } else {
        None
      }
    } finally {
      if (succLock) {
        socketLock.unlock()
      }
    }
  }

  private def send(request: HttpRequest): Unit = {
    lockAndRun {
      val headerBlock =
        ((request.method + " " + request.path + " " + request.proto + "\n") +
          ("HOST: " + request.host + "\n") +
          ("User-Agent: " + request.`User-Agent` + "\n") +
          ("Accept: " + request.Accept + "\n") +
          (request.otherHeaders.map { case (k, v) => k + ": " + v }.mkString("\n")) +
          "\n" + (if (request.body.nonEmpty) "\n" else "")
          ).getBytes(StandardCharsets.UTF_8)

      val sendBB = ByteBuffer.allocate(headerBlock.size + request.body.size)
      sendBB.put(headerBlock)

      val bodyArr = request.body.toArray
      sendBB.put(bodyArr)
      sendBB.flip()

      socketLogger.send(sendBB)
      socketChannel.write(sendBB)
    }
  }

  override def stream(request: HttpRequest)(consumer: HttpResponseStream.Event => HttpResponseStream.Behavior): Unit = {
    lockAndRun {
      send(request)
      HttpResponseStream(
        SocketChannelSupplier(socketChannel),
        sourceTimeout = socketReadTimings.streamSourceTimeout,
        readTimeout = socketReadTimings.streamReadTimeout,
        cpuThrottling = socketReadTimings.cpuThrottling,
        pid = request.id
      )
    }
  }

  class HttpResponseBuilder(pid:Long, consumer:Either[Event.Error,HttpResponse]=>Unit) extends Function1[HttpResponseStream.Event, HttpResponseStream.Behavior] {
    private var firstLine:Option[String] = None
    private var headers:List[(String,String)] = List()
    private var bodyEndAccepted = false
    private val body = new ByteArrayOutputStream()

    override def apply(event: HttpResponseStream.Event): HttpResponseStream.Behavior = {
      event match {
        case err: Event.Error =>
          consumer(Left(err))
          HttpResponseStream.Behavior.Stop
        case Event.FirstLine(pid, string) => firstLine = Some(string); HttpResponseStream.Behavior.Continue
        case Event.Header(pid, name, value) => headers = headers :+ (name,value); HttpResponseStream.Behavior.Continue
        case Event.HeaderEnd(pid) => HttpResponseStream.Behavior.Continue
        case Event.Data(pid, bytes) => body.write(bytes); HttpResponseStream.Behavior.Continue
        case Event.DataEnd(pid) =>
          bodyEndAccepted = true;
          consumer(build)
          HttpResponseStream.Behavior.Continue
      }
    }

    private def build:Either[Event.Error,HttpResponse] = {
      firstLine match {
        case Some(fLine) =>
          Right(HttpResponse(fLine, headers, body.toByteArray, bodyDecoded = true, pid=pid))
        case None =>
          Left(Event.Error.NoResponse(pid))
      }
    }
  }

  override def http(request: HttpRequest): Either[errors.HttpError, HttpResponse] = {
    lockAndRun {
      var respone:Option[Either[Event.Error,HttpResponse]] = None

      httpLogger.send(request)
      send(request)

      HttpResponseStream(
        SocketChannelSupplier(socketChannel),
        sourceTimeout = socketReadTimings.streamSourceTimeout,
        readTimeout = socketReadTimings.streamReadTimeout,
        cpuThrottling = socketReadTimings.cpuThrottling,
        pid = request.id
      ).read(new HttpResponseBuilder(request.id, et => {
        respone = Some(et)
      }))

      respone.map {
        case Left(err) =>
          Left(errors.HttpError.HttpResponseParse(err))
        case Right(resp) =>
          httpLogger.receive(resp)
          Right(resp)
      }.getOrElse(
        Left(errors.HttpError.HttpResponseNotParsed())
      )
    }
  }

  override def text(request: HttpRequest, responseWrapper: HttpResponse => HttpResponse, successHttpCode: HttpResponse => Boolean): Either[errors.HttpError, String] = {
    lockAndRun {
      for {
        response0 <- http(request)
        response = responseWrapper(response0)
        _ <- if (successHttpCode(response)) {
          Right(response)
        } else {
          Left(errors.HttpError.HttpStatusCodeNotValid())
        }
        text <- response.text.map(
          s => Right(s)
        ).getOrElse(
          Left(errors.HttpError.CantExtractText())
        )
      } yield text
    }
  }

  override def json[A: JsonReader](
                                    request: HttpRequest,
                                    responseWrapper: HttpResponse => HttpResponse,
                                    successHttpCode: HttpResponse => Boolean
                                  )(implicit ct: ClassTag[A]): Either[errors.HttpError, A] = {
    text(request,responseWrapper,successHttpCode).flatMap{ bodyText =>
      val cname = ct.runtimeClass.toGenericString
      bodyText.jsonAs[A].left.map { readerErr =>
        errors.HttpError.CantExtractJson(bodyText, cname, readerErr)
      }
    }
  }
}