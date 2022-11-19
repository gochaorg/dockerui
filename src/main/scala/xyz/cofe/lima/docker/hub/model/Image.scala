package xyz.cofe.lima.docker.hub.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class Image(
                  name: String,
                  namespace: String,
                  repository_type: Option[String],
                  status: Int,
                  is_private: Boolean,
                  star_count: Int,
                  pull_count: Long,
                  last_updated: String,
                  date_registered: String,
                  affiliation: String,
                  media_types: List[Option[String]]
                )

object Image {
  implicit val reader: JsonReader[Image] = jsonReader[Image]
}
