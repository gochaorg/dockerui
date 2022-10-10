package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class BlkioWeightDevice(
                              Path: String,
                              Weight: Int,
                            )
object BlkioWeightDevice {
  implicit val reader: JsonReader[BlkioWeightDevice] = jsonReader[BlkioWeightDevice]
}
