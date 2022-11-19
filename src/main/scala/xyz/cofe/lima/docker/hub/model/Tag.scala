package xyz.cofe.lima.docker.hub.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class Tag(
                creator: Option[Int],
                id: Option[Int],
                last_updated: Option[String],
                last_updater: Option[Int],
                last_updater_username: Option[String],
                name: Option[String],
                repository: Option[Int],
                full_size: Option[Long],
                v2: Option[Boolean],
                tag_status: Option[String],
                tag_last_pulled: Option[String],
                tag_last_pushed: Option[String],
                media_type: Option[String],
                digest: Option[String],
                images: List[ImageTag]
              )

object Tag {
  implicit val reader: JsonReader[Tag] = jsonReader[Tag]
}
