package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class NetworkSettingsInspect(
                                   Bridge: String,
                                   SandboxID: String,
                                   HairpinMode: Boolean,
                                   LinkLocalIPv6Address: String,
                                   LinkLocalIPv6PrefixLen: Int,
                                   //Ports
                                   SandboxKey: String,
                                   // SecondaryIPAddresses : Array of objects or null (Address)
                                   // SecondaryIPv6Addresses : Array of objects or null (Address)
                                   EndpointID: String,
                                   Gateway: String,
                                   GlobalIPv6Address: String,
                                   GlobalIPv6PrefixLen: Int,
                                   IPAddress: String,
                                   IPPrefixLen: Int,
                                   IPv6Gateway: String,
                                   MacAddress: String,
                                   // Networks:
                                 )
object NetworkSettingsInspect {
  implicit val reader: JsonReader[NetworkSettingsInspect] = jsonReader[NetworkSettingsInspect]
  implicit val writer: JsonWriter[NetworkSettingsInspect] = jsonWriter[NetworkSettingsInspect]

}
