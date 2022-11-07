package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class Mounts(
                   Name: Option[String],
                   Source: Option[String],
                   Destination: Option[String],
                   Driver: Option[String],
                   Mode: Option[String],
                   RW: Option[Boolean],
                   Propagation: Option[String],
                 )
object Mounts {
  implicit val reader: JsonReader[Mounts] = jsonReader[Mounts]
  implicit val writer: JsonWriter[Mounts] = jsonWriter[Mounts]
}
