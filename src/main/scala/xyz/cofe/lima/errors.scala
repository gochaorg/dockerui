package xyz.cofe.lima

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

object errors {

  //#region Throwable wrapper

  case class StackTraceElementView
  (className: Option[String]
   , fileName: Option[String]
   , classLoaderName: Option[String]
   , lineNumber: Option[Int]
   , methodName: Option[String]
   , moduleName: Option[String]
   , moduleVersion: Option[String]
   , nativeMethod: Boolean
  ) {
    def toStackTraceElement =
      new java.lang.StackTraceElement(
        classLoaderName.orNull,
        moduleName.orNull,
        moduleVersion.orNull,
        className.orNull,
        methodName.orNull,
        fileName.orNull,
        lineNumber.getOrElse(if (nativeMethod) -2 else -1)
      )
  }

  object StackTraceElementView {
    def from(el: StackTraceElement): StackTraceElementView =
      StackTraceElementView(
        Option(el.getClassName),
        Option(el.getFileName),
        Option(el.getClassLoaderName),
        Option(el.getLineNumber),
        Option(el.getMethodName),
        Option(el.getModuleName),
        Option(el.getModuleVersion),
        el.isNativeMethod
      )

    implicit val reader: JsonReader[StackTraceElementView] = jsonReader[StackTraceElementView]
    implicit val writer: JsonWriter[StackTraceElementView] = jsonWriter[StackTraceElementView]
  }

  case class ThrowableView
  (
    exceptionClassName: String,
    message: Option[String],
    localizedMessage: Option[String],
    stackTrace: List[StackTraceElementView],
    cause: Option[ThrowableView],
    suppressed: List[ThrowableView]
  ) extends Error {
    override def getMessage: String = message.orNull

    override def getLocalizedMessage: String = localizedMessage.orNull

    override def getStackTrace: Array[StackTraceElement] = {
      stackTrace.map(e => e.toStackTraceElement).toArray
    }

    override def getCause: Throwable = cause.orNull
  }

  object ThrowableView {
    def from(th: Throwable): ThrowableView = {
      ThrowableView(
        th.getClass.getName,
        Option(th.getMessage),
        Option(th.getLocalizedMessage),
        {
          val errs = th.getStackTrace
          if (errs == null) {
            List()
          } else {
            errs.toList.map(StackTraceElementView.from)
          }
        },
        {
          val err = th.getCause
          if (err == null) {
            None
          } else {
            Some(ThrowableView.from(err))
          }
        },
        {
          val sup = th.getSuppressed
          if (sup == null) {
            List()
          } else {
            sup.map(ThrowableView.from).toList
          }
        }
      )
    }

    implicit val reader: JsonReader[ThrowableView] = jsonReader[ThrowableView]
    implicit val writer: JsonWriter[ThrowableView] = jsonWriter[ThrowableView]
  }

  //#endregion

  sealed abstract class NonStackError(message: String, cause: Throwable) extends Error(message, cause, false, false)

  trait AppError {
    def message:String
  }

  trait DockerClientError extends AppError
  object DockerClientError {
    case class Simple(message:String) extends DockerClientError
  }

  sealed trait HttpError extends AppError
  object HttpError {
    case class BadHeaderFormat(message:String) extends HttpError
    case class HeaderExpected(message:String = "No response: header not readed") extends HttpError
    case class NoData(message:String="no data") extends HttpError
    case class TransferEncodingExpectLF(message:String="expect LF (\\n)") extends HttpError
    case class TransferEncodingExpectCR(message:String="expect CR (\\n)") extends HttpError
    case class TransferEncodingNoData(expect:Long, actual:Long, message:String="expect CR (\\n)") extends HttpError
    case class TransferEncodingExpectCRLF(b0:Byte, b1:Byte, message:String="expect CR (\\n)") extends HttpError
  }
}
