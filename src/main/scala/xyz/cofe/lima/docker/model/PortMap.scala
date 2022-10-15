package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class PortMap(
                    IP: Option[String],
                    PrivatePort: Option[Int],
                    PublicPort: Option[Int],
                    Type: String
                  )
object PortMap {
  implicit val reader: JsonReader[PortMap] = jsonReader[PortMap]
  implicit val writer: JsonWriter[PortMap] = jsonWriter[PortMap]
}
