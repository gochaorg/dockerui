package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class Ulimits(
                    Name: String,
                    Soft: Int,
                    Hard: Int,
                  )
object Ulimits {
  implicit val reader: JsonReader[Ulimits] = jsonReader[Ulimits]
}
