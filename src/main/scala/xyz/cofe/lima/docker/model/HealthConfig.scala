package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class HealthConfig(
                         Test: List[String],
                         Interval: Int,
                         Timeout: Int,
                         Retries: Int,
                         StartPeriod:Int,
                       )
object HealthConfig{
  implicit val reader: JsonReader[HealthConfig] = jsonReader[HealthConfig]
}
