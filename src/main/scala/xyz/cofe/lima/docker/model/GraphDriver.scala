package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class GraphDriver(Name:String, Data:Map[String,String])
object GraphDriver {
  implicit val reader: JsonReader[GraphDriver] = jsonReader[GraphDriver]
  implicit val writer: JsonWriter[GraphDriver] = jsonWriter[GraphDriver]
}
