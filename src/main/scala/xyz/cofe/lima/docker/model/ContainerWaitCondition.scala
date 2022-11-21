package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import xyz.cofe.lima.docker.http.HttpParamValue

sealed trait ContainerWaitCondition

object ContainerWaitCondition {
  case object NotRunning extends ContainerWaitCondition
  case object NextExit extends ContainerWaitCondition
  case object Removed extends ContainerWaitCondition
  case object UNDEFINED extends ContainerWaitCondition

  val jsonStringReader : JsonReader[String] = implicitly[JsonReader[String]]
  val jsonStringWriter : JsonWriter[String] = implicitly[JsonWriter[String]]

  implicit val reader: JsonReader[ContainerWaitCondition] = jsonStringReader.map {
    case "NotRunning" => NotRunning
    case "NextExit" => NextExit
    case "Removed" => Removed
    case _ => UNDEFINED
  }

  implicit val writer: JsonWriter[ContainerWaitCondition] = jsonStringWriter.contramap {
    case NotRunning => "NotRunning"
    case NextExit => "NextExit"
    case Removed => "Removed"
    case UNDEFINED => "UNDEFINED"
  }

  implicit val httpParam: HttpParamValue[ContainerWaitCondition] = {
    case NotRunning => Some("not-running")
    case NextExit => Some("next-exit")
    case Removed => Some("removed")
    case UNDEFINED => None
  }
}