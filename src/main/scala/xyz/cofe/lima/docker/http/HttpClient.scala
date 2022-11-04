package xyz.cofe.lima.docker.http

import tethys.JsonReader

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import java.util.concurrent.locks.{Lock, ReentrantLock}

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
    ): Either[ERROR, A]
}

class HttpClientImpl
  (
    val socketChannel: SocketChannel,
    val socketLock: Lock = new ReentrantLock(),
    val socketTiming: SocketReadTimings = SocketReadTimings()
  )(implicit
    val socketLogger: SocketLogger
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

      )
    }
  }

  override def http(request: HttpRequest): Either[errors.HttpError, HttpResponse] = ???

  override def text(request: HttpRequest, responseWrapper: HttpResponse => HttpResponse, successHttpCode: HttpResponse => Boolean): Either[errors.HttpError, String] = ???

  override def json[A: JsonReader](request: HttpRequest, responseWrapper: HttpResponse => HttpResponse, successHttpCode: HttpResponse => Boolean): Either[errors.HttpError, A] = ???
}