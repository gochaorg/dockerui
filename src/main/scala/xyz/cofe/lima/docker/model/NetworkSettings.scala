package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class NetworkSettings(
                            Networks: Map[String,Network]
                          )
object NetworkSettings {
  implicit val reader: JsonReader[NetworkSettings] = jsonReader[NetworkSettings]
}

