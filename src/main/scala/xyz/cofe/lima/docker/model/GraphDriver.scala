package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class GraphDriver(Name:String, Data:Map[String,String])
object GraphDriver {
  implicit val reader: JsonReader[GraphDriver] = jsonReader[GraphDriver]
}
