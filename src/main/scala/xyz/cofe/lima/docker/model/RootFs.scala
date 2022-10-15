package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class RootFs(Type:String,Layers:List[String])
object RootFs {
  implicit val reader: JsonReader[RootFs] = jsonReader[RootFs]
  implicit val writer: JsonWriter[RootFs] = jsonWriter[RootFs]
}
