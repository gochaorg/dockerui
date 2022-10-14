package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class Top(
                Titles: List[String],
                Processes: List[List[String]]
              )
object Top {
  implicit val reader: JsonReader[Top] = jsonReader[Top]
  implicit val writer: JsonWriter[Top] = jsonWriter[Top]
}
