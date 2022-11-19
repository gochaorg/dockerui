package xyz.cofe.lima.docker.hub.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class Tags(count: Int, next: Option[String], previous: Option[String], results: List[Tag])

object Tags {
  implicit val reader: JsonReader[Tags] = jsonReader[Tags]
}
