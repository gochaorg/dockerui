package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class HealthConfig(
                         Test: List[String],
                         Interval: Int,
                         Timeout: Int,
                         Retries: Int,
                         StartPeriod:Int,
                       )
object HealthConfig{
  implicit val reader: JsonReader[HealthConfig] = jsonReader[HealthConfig]
  implicit val writer: JsonWriter[HealthConfig] = jsonWriter[HealthConfig]
}
