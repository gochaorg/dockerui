package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class ThrottleDevice(
                           Path: String,
                           Rate: Double
                         )
object ThrottleDevice{
  implicit val reader: JsonReader[ThrottleDevice] = jsonReader[ThrottleDevice]
}
