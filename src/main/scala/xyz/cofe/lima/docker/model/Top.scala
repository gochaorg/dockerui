package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class Top(
                Titles: List[String],
                Processes: List[List[String]]
              )
object Top {
  implicit val reader: JsonReader[Top] = jsonReader[Top]
}
