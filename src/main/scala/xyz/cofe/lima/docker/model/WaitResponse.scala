package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class WaitResponse(StatusCode:Int, Error:Option[WaitResponseError])
object WaitResponse {
  implicit val reader: JsonReader[WaitResponse] = jsonReader[WaitResponse]
  implicit val writer: JsonWriter[WaitResponse] = jsonWriter[WaitResponse]
}

case class WaitResponseError(Message:Option[String])
object WaitResponseError {
  implicit val reader: JsonReader[WaitResponseError] = jsonReader[WaitResponseError]
  implicit val writer: JsonWriter[WaitResponseError] = jsonWriter[WaitResponseError]
}