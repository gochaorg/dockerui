package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class Mounts(
                   Name: String,
                   Source: String,
                   Destination: String,
                   Driver: String,
                   Mode: String,
                   RW: Boolean,
                   Propagation: String,
                 )
object Mounts {
  implicit val reader: JsonReader[Mounts] = jsonReader[Mounts]
  implicit val writer: JsonWriter[Mounts] = jsonWriter[Mounts]
}
