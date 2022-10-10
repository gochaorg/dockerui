package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class Network(
                    IPAMConfig: Option[IPAMConfig],
                    Links: Option[List[String]],
                    Aliases: Option[List[String]],
                    NetworkID: String,
                    EndpointID: Option[String],
                    Gateway: Option[String],
                    IPAddress: Option[String],
                    IPPrefixLen: Option[Int],
                    IPv6Gateway: Option[String],
                    GlobalIPv6Address: Option[String],
                    GlobalIPv6PrefixLen: Option[Long],
                    MacAddress:String,
                  )
object Network {
  implicit val reader: JsonReader[Network] = jsonReader[Network]
}
