package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class ImageRemove(Untagged:Option[String], Deleted:Option[String])
object ImageRemove {
  implicit val reader: JsonReader[ImageRemove] = jsonReader[ImageRemove]
  implicit val writer: JsonWriter[ImageRemove] = jsonWriter[ImageRemove]
}
