package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class Ulimits(
                    Name: String,
                    Soft: Int,
                    Hard: Int,
                  )
object Ulimits {
  implicit val reader: JsonReader[Ulimits] = jsonReader[Ulimits]
  implicit val writer: JsonWriter[Ulimits] = jsonWriter[Ulimits]
}
