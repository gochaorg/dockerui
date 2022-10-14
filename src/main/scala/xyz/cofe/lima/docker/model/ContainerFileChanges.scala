package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class ContainerFileChanges(Path:String, Kind:Int) {
  import ContainerFileChanges._
  lazy val changes:Option[Changes] = Kind match {
    case 0 => Some(Modified(Path))
    case 1 => Some(Added(Path))
    case 2 => Some(Deleted(Path))
    case _ => None
  }
}
object ContainerFileChanges {
  sealed trait Changes
  case class Modified(path:String) extends Changes
  case class Added(path:String) extends Changes
  case class Deleted(path:String) extends Changes

  implicit val reader: JsonReader[ContainerFileChanges] = jsonReader[ContainerFileChanges]
}
