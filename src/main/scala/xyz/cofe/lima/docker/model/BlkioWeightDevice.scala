package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class BlkioWeightDevice(
                              Path: String,
                              Weight: Int,
                            )
object BlkioWeightDevice {
  implicit val reader: JsonReader[BlkioWeightDevice] = jsonReader[BlkioWeightDevice]
  implicit val writer: JsonWriter[BlkioWeightDevice] = jsonWriter[BlkioWeightDevice]
}
