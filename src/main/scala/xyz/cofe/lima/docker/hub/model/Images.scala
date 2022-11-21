package xyz.cofe.lima.docker.hub.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class Images(
                   count: Int,
                   next: Option[String],
                   previous: Option[String],
                   results: List[Image]
                 )

object Images {
  implicit val reader: JsonReader[Images] = jsonReader[Images]
}
