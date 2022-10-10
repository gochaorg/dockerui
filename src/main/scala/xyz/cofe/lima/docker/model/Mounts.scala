package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class Mounts(
                   Name: String,
                   Source: String,
                   Destination: String,
                   Driver: String,
                   Mode: String,
                   RW: Boolean,
                   Propagation: String,
                 )
object Mounts {
  implicit val reader: JsonReader[Mounts] = jsonReader[Mounts]
}
