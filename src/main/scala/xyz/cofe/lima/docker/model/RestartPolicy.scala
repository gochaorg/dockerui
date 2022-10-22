package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class RestartPolicy(
  // Enum: "" "no" "always" "unless-stopped" "on-failure"
  //    Empty - string means not to restart
  //    no - Do not automatically restart
  //    always - Always restart
  //    unless-stopped - Restart always except when the user has manually stopped the container
  //    on-failure - Restart only when the container exit code is non-zero
  Name: String,

  // If on-failure is used, the number of times to retry before giving up.
  MaximumRetryCount: Int
)
object RestartPolicy {
  implicit val reader: JsonReader[RestartPolicy] = jsonReader[RestartPolicy]
  implicit val writer: JsonWriter[RestartPolicy] = jsonWriter[RestartPolicy]
}
