package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class ContainerState(
                           /** "created" "running" "paused" "restarting" "removing" "exited" "dead" */
                           Status: String,
                           Running: Boolean,
                           Paused: Boolean,
                           Restarting: Boolean,
                           OOMKilled: Boolean,
                           Dead: Boolean,
                           Pid: Int,
                           ExitCode: Int,
                           Error: String,

                           /** The time when this container was last started : 2015-01-06T15:47:32.072697474Z */
                           StartedAt: String,

                           /** The time when this container last exited : 2015-01-06T15:47:32.080254511Z */
                           FinishedAt: String,

                           Health: Option[Health],
                         )
object ContainerState {
  implicit val reader: JsonReader[ContainerState] = jsonReader[ContainerState]
  implicit val writer: JsonWriter[ContainerState] = jsonWriter[ContainerState]
}
