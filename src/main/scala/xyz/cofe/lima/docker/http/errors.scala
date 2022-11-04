package xyz.cofe.lima.docker.http

import xyz.cofe.lima.errors.AppError

object errors {
  sealed trait HttpError extends AppError

  object HttpError {
    case class SocketSendError(error:Throwable)  extends HttpError { val message:String = s"socket i/o error: ${error.getMessage}" }
    case class BadHeaderFormat(message: String) extends HttpError
    case class HeaderExpected(message: String = "No response: header not readed") extends HttpError
    case class NoData(message: String = "no data") extends HttpError
    case class TransferEncodingExpectLF(message: String = "expect LF (\\n)") extends HttpError
    case class TransferEncodingExpectCR(message: String = "expect CR (\\n)") extends HttpError
    case class TransferEncodingNoData(expect: Long, actual: Long, message: String = "expect CR (\\n)") extends HttpError
    case class TransferEncodingExpectCRLF(b0: Byte, b1: Byte, message: String = "expect CR (\\n)") extends HttpError
  }
}
