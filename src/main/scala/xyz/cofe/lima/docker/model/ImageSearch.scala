package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class ImageSearch(
                        description:Option[String],
                        is_official:Option[Boolean],
                        is_automated:Option[Boolean],
                        name:Option[String],
                        star_count:Option[Int]
                      )
object ImageSearch {
  implicit val reader: JsonReader[ImageSearch] = jsonReader[ImageSearch]
  implicit val writer: JsonWriter[ImageSearch] = jsonWriter[ImageSearch]
}
