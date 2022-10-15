package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class NetworkSettings(
                            Networks: Map[String,Network]
                          )
object NetworkSettings {
  implicit val reader: JsonReader[NetworkSettings] = jsonReader[NetworkSettings]
  implicit val writer: JsonWriter[NetworkSettings] = jsonWriter[NetworkSettings]
}

