package xyz.cofe.lima.docker.http

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.docker.http.HttpResponseStream.Event
import xyz.cofe.lima.errors.{AppError, ThrowableView}
import xyz.cofe.lima.store.json._

object errors {
  sealed trait HttpError extends AppError

  object HttpError {
    case class SocketSend(error:Throwable)  extends HttpError { val message:String = s"socket i/o error: ${error.getMessage}" }
    object SocketSend {
      case class SocketSendView(error:ThrowableView, `_type`:String="SocketSend")
      object SocketSendView {
        implicit val writer: JsonWriter[SocketSendView] = jsonWriter[SocketSendView]
        implicit val reader: JsonReader[SocketSendView] = jsonReader[SocketSendView]
      }

      implicit val writer:JsonWriter[SocketSend] = SocketSendView.writer.contramap( obj => SocketSendView(ThrowableView.from(obj.error)) )
      implicit val reader:JsonReader[SocketSend] = SocketSendView.reader.map( obj => SocketSend(obj.error) )
    }

    case class HttpResponseParse(err:Event.Error) extends HttpError {
      val message:String = err.errorMessage
    }
    object HttpResponseParse {
      implicit val writer: JsonWriter[HttpResponseParse] = classWriter[HttpResponseParse] ++ jsonWriter[HttpResponseParse]
      implicit val reader: JsonReader[HttpResponseParse] = jsonReader[HttpResponseParse]
    }

    case class HttpResponseNotParsed() extends HttpError {
      val message:String = "Response not parsed"
    }
    object HttpResponseNotParsed {
      implicit val writer: JsonWriter[HttpResponseNotParsed] = classWriter[HttpResponseNotParsed] ++ jsonWriter[HttpResponseNotParsed]
      implicit val reader: JsonReader[HttpResponseNotParsed] = jsonReader[HttpResponseNotParsed]
    }

    case class HttpStatusCodeNotValid() extends HttpError {
      val message:String = s"http not valid status code by user function"
    }
    object HttpStatusCodeNotValid {
      implicit val writer: JsonWriter[HttpStatusCodeNotValid] = classWriter[HttpStatusCodeNotValid] ++ jsonWriter[HttpStatusCodeNotValid]
      implicit val reader: JsonReader[HttpStatusCodeNotValid] = jsonReader[HttpStatusCodeNotValid]
    }

    case class CantExtractText() extends HttpError {
      val message:String = s"can't extract text from body of response"
    }
    object CantExtractText {
      implicit val writer: JsonWriter[CantExtractText] = classWriter[CantExtractText] ++ jsonWriter[CantExtractText]
      implicit val reader: JsonReader[CantExtractText] = jsonReader[CantExtractText]
    }

    case class CantExtractJson(bodyText:String, jsonClass:String, jsonParseError:Throwable) extends HttpError {
      val message:String = s"can't parse json for $jsonClass: ${jsonParseError.getMessage}"
    }
    object CantExtractJson {
      case class CantExtractJsonView(bodyText:String, jsonClass:String, jsonParseError:ThrowableView, `_type`:String="CantExtractJson")
      object CantExtractJsonView {
        implicit val writer: JsonWriter[CantExtractJsonView] = jsonWriter[CantExtractJsonView]
        implicit val reader: JsonReader[CantExtractJsonView] = jsonReader[CantExtractJsonView]
      }

      implicit val writer: JsonWriter[CantExtractJson] = CantExtractJsonView.writer.contramap( obj => CantExtractJsonView(obj.bodyText, obj.jsonClass, ThrowableView.from(obj.jsonParseError)) )
      implicit val reader: JsonReader[CantExtractJson] = CantExtractJsonView.reader.map( obj => CantExtractJson(obj.bodyText, obj.jsonClass, obj.jsonParseError) )
    }

    implicit val writer:JsonWriter[HttpError] = (value: HttpError, tokenWriter: TokenWriter) => value match {
      case ev: SocketSend => SocketSend.writer.write(ev, tokenWriter)
      case ev: HttpResponseParse => HttpResponseParse.writer.write(ev, tokenWriter)
      case ev: HttpResponseNotParsed => HttpResponseNotParsed.writer.write(ev, tokenWriter)
      case ev: HttpStatusCodeNotValid => HttpStatusCodeNotValid.writer.write(ev, tokenWriter)
      case ev: CantExtractText => CantExtractText.writer.write(ev, tokenWriter)
      case ev: CantExtractJson => CantExtractJson.writer.write(ev, tokenWriter)
    }
    implicit val reader:JsonReader[HttpError] =  JsonReader.builder.addField[String]("_type").selectReader[HttpError] {
      case "SocketSend" => SocketSend.reader
      case "HttpResponseParse" => HttpResponseParse.reader
      case "HttpResponseNotParsed" => HttpResponseNotParsed.reader
      case "HttpStatusCodeNotValid" => HttpStatusCodeNotValid.reader
      case "CantExtractText" => CantExtractText.reader
      case "CantExtractJson" => CantExtractJson.reader
    }
  }
}
