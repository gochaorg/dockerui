package xyz.cofe.lima.docker

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.docker.model.ErrorResponse
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
  case class UnExpectedStatusCode(response:http.HttpResponse, expectStatusCodes:Seq[Int]) extends DockerError
  {
    val message=s"UnExpectedStatusCode ${response.code}, expected ${expectStatusCodes}"
  }

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

  /**
   * Ощая ошибка - хз какая
   * @param message сообщение
   */
  case class GenericErr(message:String) extends DockerError
  object GenericErr {
    def apply(message: String): GenericErr = new GenericErr(message)
    def apply(message: ErrorResponse): GenericErr = new GenericErr(message.message)

    implicit val reader:JsonReader[GenericErr] = jsonReader[GenericErr]
    implicit val writer:JsonWriter[GenericErr] = classWriter[GenericErr] ++ jsonWriter[GenericErr]
  }

  /**
   * ошибка запроса, касячныые параметры
   * @param message сообщение от docker
   */
  case class BadRequest(message:String) extends DockerError
  object BadRequest {
    def apply(message:String):BadRequest = new BadRequest(message)
    def apply(message:ErrorResponse):BadRequest = new BadRequest(message.message)
    implicit val reader:JsonReader[BadRequest] = jsonReader[BadRequest]
    implicit val writer:JsonWriter[BadRequest] = classWriter[BadRequest] ++ jsonWriter[BadRequest]
  }

  /**
   * какой-то конфликт состояния docker
   * @param message сообщение от docker
   */
  case class Conflict(message:String) extends DockerError
  object Conflict {
    def apply(message:String):Conflict = new Conflict(message)
    def apply(message:ErrorResponse):Conflict = new Conflict(message.message)
    implicit val reader:JsonReader[Conflict] = jsonReader[Conflict]
    implicit val writer:JsonWriter[Conflict] = classWriter[Conflict] ++ jsonWriter[Conflict]
  }

  /**
   * не найден контейнер
   * @param message сообщение от docker
   */
  case class NotFound(message:String) extends DockerError
  object NotFound {
    def apply(message: String): NotFound = new NotFound(message)
    def apply(message: ErrorResponse): NotFound = new NotFound(message.message)

    implicit val reader:JsonReader[NotFound] = jsonReader[NotFound]
    implicit val writer:JsonWriter[NotFound] = classWriter[NotFound] ++ jsonWriter[NotFound]
  }

  /**
   * что-то докер себя повел не так
   * @param message сообщение от docker
   */
  case class ServerErr(message:String) extends DockerError
  object ServerErr {
    def apply(message: String): ServerErr = new ServerErr(message)
    def apply(message: ErrorResponse): ServerErr = new ServerErr(message.message)

    implicit val reader:JsonReader[ServerErr] = jsonReader[ServerErr]
    implicit val writer:JsonWriter[ServerErr] = classWriter[ServerErr] ++ jsonWriter[ServerErr]
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
      case e:NotFound => NotFound.writer.write(e,tokenWriter)
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
    case "NotFound" => NotFound.reader
  }
}
