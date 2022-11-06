package xyz.cofe.lima.store.config

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import xyz.cofe.lima.docker.http.SocketReadTimings

case class AppConfig
(
  dockerConnect: DockerConnect,
)
object AppConfig {
  implicit val reader:JsonReader[AppConfig] = jsonReader[AppConfig]
  implicit val writer:JsonWriter[AppConfig] = jsonWriter[AppConfig]
}

sealed trait DockerConnect
case class UnixSocketFile
(
  fileName:String,
  socketReadTimings: SocketReadTimings
) extends DockerConnect
object UnixSocketFile {
  implicit val reader: JsonReader[UnixSocketFile] = jsonReader[UnixSocketFile]
  implicit val writer: JsonWriter[UnixSocketFile] = jsonWriter[UnixSocketFile]
}

