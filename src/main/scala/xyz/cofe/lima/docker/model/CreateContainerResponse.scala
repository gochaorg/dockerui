package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class CreateContainerResponse(
                                    Id: String,
                                    Warnings: List[String]
                                  )
object CreateContainerResponse {
  implicit val writer: JsonWriter[CreateContainerResponse] = jsonWriter[CreateContainerResponse]
  implicit val reader: JsonReader[CreateContainerResponse] = jsonReader[CreateContainerResponse]
}

