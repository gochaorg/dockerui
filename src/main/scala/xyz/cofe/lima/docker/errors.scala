package xyz.cofe.lima.docker

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.errors.{AppError, ThrowableView}
import xyz.cofe.lima.store.json._

object errors {
  sealed trait DockerError extends AppError

  /**
   * код ответа не присуствует в ответе
   * @param response http ответ
   */
  case class StatusCodeNotAvailable(response:http.HttpResponse) extends DockerError { val message="StatusCodeNotAvailable" }
  object StatusCodeNotAvailable {
    implicit val reader: JsonReader[StatusCodeNotAvailable] = jsonReader[StatusCodeNotAvailable]
    implicit val writer: JsonWriter[StatusCodeNotAvailable] = classWriter[StatusCodeNotAvailable] ++ jsonWriter[StatusCodeNotAvailable]
  }

  /**
   * Не ожиданный код ответа
   * @param response ответ
   */
  case class UnExpectedStatusCode(response:http.HttpResponse, expectStatusCode:Int) extends DockerError { val message=s"UnExpectedStatusCode ${response.code}" }
  object UnExpectedStatusCode {
    implicit val reader: JsonReader[UnExpectedStatusCode] = jsonReader[UnExpectedStatusCode]
    implicit val writer: JsonWriter[UnExpectedStatusCode] = classWriter[UnExpectedStatusCode] ++ jsonWriter[UnExpectedStatusCode]
  }

  /**
   * Текст не извлечен из ответа
   * @param response ответ
   */
  case class CantExtractText(response:http.HttpResponse) extends DockerError { val message=s"CantExtractText ${response.code}" }
  object CantExtractText {
    implicit val reader: JsonReader[CantExtractText] = jsonReader[CantExtractText]
    implicit val writer: JsonWriter[CantExtractText] = classWriter[CantExtractText] ++ jsonWriter[CantExtractText]
  }

  /**
   * Текст не извлечен из ответа
   * @param response ответ
   */
  case class CantExtractJson(bodyText:String, className:String, jsonParseErr:Throwable) extends DockerError { val message=s"CantExtractJson for $className" }
  object CantExtractJson {
    case class CantExtractJsonView(bodyText:String, className:String, jsonParseErr:ThrowableView, `_type`:String="CantExtractJson")
    object CantExtractJsonView {
      implicit val reader: JsonReader[CantExtractJsonView] = jsonReader[CantExtractJsonView]
      implicit val writer: JsonWriter[CantExtractJsonView] = jsonWriter[CantExtractJsonView]
    }
    implicit val reader: JsonReader[CantExtractJson] = CantExtractJsonView.reader.map( v => CantExtractJson(v.bodyText, v.className, v.jsonParseErr) )
    implicit val writer: JsonWriter[CantExtractJson] = CantExtractJsonView.writer.contramap( v => CantExtractJsonView(v.bodyText, v.className, ThrowableView.from(v.jsonParseErr)) )
  }

  case class GenericErr(message:String) extends DockerError
  object GenericErr {
    implicit val reader:JsonReader[GenericErr] = jsonReader[GenericErr]
    implicit val writer:JsonWriter[GenericErr] = classWriter[GenericErr] ++ jsonWriter[GenericErr]
  }

  case class BadRequest(message:String) extends DockerError
  object BadRequest {
    implicit val reader:JsonReader[BadRequest] = jsonReader[BadRequest]
    implicit val writer:JsonWriter[BadRequest] = classWriter[BadRequest] ++ jsonWriter[BadRequest]
  }

  /**
   * Ошибка кодирования http / ошибка передачи данных
   * @param err ошибка
   */
  case class HttpErr(err: http.errors.HttpError) extends DockerError {
    val message = s"HttpErr ${err.message}"
  }
  object HttpErr {
    implicit val reader: JsonReader[HttpErr] = jsonReader[HttpErr]
    implicit val writer: JsonWriter[HttpErr] = classWriter[HttpErr] ++ jsonWriter[HttpErr]
  }

  implicit val writer:JsonWriter[DockerError] = (value: DockerError, tokenWriter: TokenWriter) => {
    value match {
      case e:GenericErr => GenericErr.writer.write(e,tokenWriter)
      case e:HttpErr => HttpErr.writer.write(e,tokenWriter)
      case e:StatusCodeNotAvailable => StatusCodeNotAvailable.writer.write(e,tokenWriter)
      case e:UnExpectedStatusCode => UnExpectedStatusCode.writer.write(e,tokenWriter)
      case e:CantExtractText => CantExtractText.writer.write(e,tokenWriter)
      case e:CantExtractJson => CantExtractJson.writer.write(e,tokenWriter)
      case e:BadRequest => BadRequest.writer.write(e,tokenWriter)
    }
  }
  implicit val reader:JsonReader[DockerError] = JsonReader.builder.addField[String]("_type").selectReader[DockerError] {
    case "GenericErr" => GenericErr.reader
    case "HttpErr" => HttpErr.reader
    case "StatusCodeNotAvailable" => StatusCodeNotAvailable.reader
    case "UnExpectedStatusCode" => UnExpectedStatusCode.reader
    case "CantExtractText" => CantExtractText.reader
    case "CantExtractJson" => CantExtractJson.reader
    case "BadRequest" => BadRequest.reader
  }
}
