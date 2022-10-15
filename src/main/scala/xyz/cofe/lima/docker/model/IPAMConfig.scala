package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}


case class IPAMConfig(
                       IPv4Address: String,
                       IPv6Address: String,
                       LinkLocalIPs: List[String]
                     )
object IPAMConfig {
  implicit val reader: JsonReader[IPAMConfig] = jsonReader[IPAMConfig]
  implicit val writer: JsonWriter[IPAMConfig] = jsonWriter[IPAMConfig]
}
