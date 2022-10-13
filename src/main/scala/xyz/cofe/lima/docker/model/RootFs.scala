package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class RootFs(Type:String,Layers:List[String])
object RootFs {
  implicit val reader: JsonReader[RootFs] = jsonReader[RootFs]
}
