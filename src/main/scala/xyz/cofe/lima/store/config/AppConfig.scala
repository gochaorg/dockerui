package xyz.cofe.lima.store.config

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class AppConfig()
object AppConfig {
  implicit val reader:JsonReader[AppConfig] = jsonReader[AppConfig]
  implicit val writer:JsonWriter[AppConfig] = jsonWriter[AppConfig]
}

sealed trait DockerConnect
case class UnixSocketFile( fileName:String ) extends DockerConnect