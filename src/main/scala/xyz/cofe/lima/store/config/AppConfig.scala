package xyz.cofe.lima.store.config

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.docker.http.SocketReadTimings
import xyz.cofe.lima.store.json._

case class AppConfig
(
  dockerConnect: DockerConnect,
)
object AppConfig {
  implicit val reader:JsonReader[AppConfig] = jsonReader[AppConfig]
  implicit val writer:JsonWriter[AppConfig] = jsonWriter[AppConfig]
}

case class UnixSocketFile
(
  fileName:String,
  socketReadTimings: SocketReadTimings
) extends DockerConnect
object UnixSocketFile {
  implicit val reader: JsonReader[UnixSocketFile] = jsonReader[UnixSocketFile]
  implicit val writer: JsonWriter[UnixSocketFile] = classWriter[UnixSocketFile] ++ jsonWriter[UnixSocketFile]
}

sealed trait DockerConnect
object DockerConnect {
  implicit val reader:JsonReader[DockerConnect] = JsonReader.builder.addField[String]("_type").selectReader[DockerConnect] {
    case "UnixSocketFile" => UnixSocketFile.reader
  }
  implicit val writer:JsonWriter[DockerConnect] = (value: DockerConnect, tokenWriter: TokenWriter) => {
    value match {
      case u: UnixSocketFile => UnixSocketFile.writer.write(u, tokenWriter)
    }
  }
}