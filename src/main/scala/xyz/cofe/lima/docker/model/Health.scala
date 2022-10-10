package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

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
}
