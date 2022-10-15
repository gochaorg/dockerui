package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class ThrottleDevice(
                           Path: String,
                           Rate: Double
                         )
object ThrottleDevice{
  implicit val reader: JsonReader[ThrottleDevice] = jsonReader[ThrottleDevice]
  implicit val writer: JsonWriter[ThrottleDevice] = jsonWriter[ThrottleDevice]
}
