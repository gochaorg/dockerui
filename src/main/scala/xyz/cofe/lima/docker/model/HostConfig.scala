package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class HostConfig(
                       NetworkMode: String
                     )
object HostConfig {
  implicit val reader: JsonReader[HostConfig] = jsonReader[HostConfig]
}
