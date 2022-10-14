package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class ImageHistory(Id:String,Created:Long,CreatedBy:String,Tags:List[String],Size:Long,Comment:String)
object ImageHistory {
  implicit val reader: JsonReader[ImageHistory] = jsonReader[ImageHistory]
  implicit val writer: JsonWriter[ImageHistory] = jsonWriter[ImageHistory]
}
