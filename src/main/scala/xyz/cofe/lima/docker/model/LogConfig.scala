package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class LogConfig(
                      Type: String,
                      Config: Option[Map[String,String]],
                    )
object LogConfig {
  implicit val reader: JsonReader[LogConfig] = jsonReader[LogConfig]
}
