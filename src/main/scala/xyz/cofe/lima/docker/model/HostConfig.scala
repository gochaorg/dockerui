package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.jsonReader
import tethys.derivation.semiauto.jsonWriter
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class HostConfig(
                       NetworkMode: String
                     )
object HostConfig {
  implicit val reader: JsonReader[HostConfig] = jsonReader[HostConfig]
  implicit val writer: JsonWriter[HostConfig] = jsonWriter[HostConfig]
}
