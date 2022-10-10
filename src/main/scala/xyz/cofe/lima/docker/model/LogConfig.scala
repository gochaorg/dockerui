package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class LogConfig(
                      Type: String,
                      Config: Option[Map[String,String]],
                    )
object LogConfig {
  implicit val reader: JsonReader[LogConfig] = jsonReader[LogConfig]
}
