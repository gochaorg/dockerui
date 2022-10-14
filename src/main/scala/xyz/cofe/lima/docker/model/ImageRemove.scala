package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class ImageRemove(Untagged:Option[String], Deleted:Option[String])
object ImageRemove {
  implicit val reader: JsonReader[ImageRemove] = jsonReader[ImageRemove]
}
