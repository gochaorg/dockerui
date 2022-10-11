package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class CreateContainerResponse(
                                    Id: String,
                                    Warnings: List[String]
                                  )
object CreateContainerResponse {
  implicit val reader: JsonReader[CreateContainerResponse] = jsonReader[CreateContainerResponse]
}

