package xyz.cofe.lima.docker.hub.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class ImageTag(
                     architecture: Option[String],
                     features: Option[String],
                     variant: Option[String],
                     digest: Option[String],
                     os: Option[String],
                     os_features: Option[String],
                     os_version: Option[String],
                     size: Option[Long],
                     status: Option[String],
                     last_pulled: Option[String],
                     last_pushed: Option[String],
                   )

object ImageTag {
  implicit val reader: JsonReader[ImageTag] = jsonReader[ImageTag]
}
