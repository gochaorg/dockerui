package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}


case class IPAMConfig(
                       IPv4Address: String,
                       IPv6Address: String,
                       LinkLocalIPs: List[String]
                     )
object IPAMConfig {
  implicit val reader: JsonReader[IPAMConfig] = jsonReader[IPAMConfig]
}
