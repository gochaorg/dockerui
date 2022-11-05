package xyz.cofe.lima.docker

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.errors.AppError
import xyz.cofe.lima.store.json._

object errors {
  sealed trait DockerError extends AppError
  case class GenericErr(message:String) extends DockerError
  object GenericErr {
    implicit val reader:JsonReader[GenericErr] = jsonReader[GenericErr]
    implicit val writer:JsonWriter[GenericErr] = classWriter[GenericErr] ++ jsonWriter[GenericErr]
  }

  implicit val writer:JsonWriter[DockerError] = (value: DockerError, tokenWriter: TokenWriter) => {
    value match {
      case e:GenericErr => GenericErr.writer.write(e,tokenWriter)
    }
  }
  implicit val reader:JsonReader[DockerError] = JsonReader.builder.addField[String]("_type").selectReader[DockerError] {
    case "GenericErr" => GenericErr.reader
  }
}
