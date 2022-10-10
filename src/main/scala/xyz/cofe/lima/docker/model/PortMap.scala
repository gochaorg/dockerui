package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class PortMap(
                    IP: Option[String],
                    PrivatePort: Option[Int],
                    PublicPort: Option[Int],
                    Type: String
                  )
object PortMap {
  implicit val reader: JsonReader[PortMap] = jsonReader[PortMap]
}
