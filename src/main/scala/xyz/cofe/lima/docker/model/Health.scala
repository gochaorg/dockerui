package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class Health(
                   /** "none" "starting" "healthy" "unhealthy" */
                   Status: String,

                   FailingStreak: Int,

                   // List[HealthcheckResult]
                   // Log contains the last few results (oldest first)
                   //Log: List[]
                 )
object Health {
  implicit val reader: JsonReader[Health] = jsonReader[Health]
  implicit val writer: JsonWriter[Health] = jsonWriter[Health]
}
