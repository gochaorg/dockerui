package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class LogConfig(
                      // "json-file" "syslog" "journald" "gelf" "fluentd" "awslogs" "splunk" "etwlogs" "none"
                      Type: String,
                      Config: Option[Map[String,String]],
                    )
object LogConfig {
  implicit val reader: JsonReader[LogConfig] = jsonReader[LogConfig]
  implicit val writer: JsonWriter[LogConfig] = jsonWriter[LogConfig]
}
