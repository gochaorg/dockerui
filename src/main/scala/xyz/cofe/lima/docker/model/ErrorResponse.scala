package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class ErrorResponse(message:String)
object ErrorResponse {
  implicit val reader: JsonReader[ErrorResponse] = jsonReader[ErrorResponse]
  implicit val writer: JsonWriter[ErrorResponse] = jsonWriter[ErrorResponse]
}
