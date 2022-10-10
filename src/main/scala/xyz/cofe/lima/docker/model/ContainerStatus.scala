package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class ContainerStatus(
                            /** The ID of this container */
                            Id:String,

                            /** The names that this container has been given */
                            Names:List[String],

                            /** The name of the image used when creating this container */
                            Image:String,

                            /** The ID of the image that this container was created from */
                            ImageID:String,

                            /** Command to run when starting the container */
                            Command:String,

                            /** When the container was created */
                            Created:Long,

                            /** The ports exposed by this container */
                            Ports:List[PortMap],

                            /** User-defined key/value metadata */
                            Labels:Map[String,String],

                            /** The state of this container */
                            State:String,

                            /** Additional human-readable status of this container */
                            Status:String,

                            /** The size of files that have been created or changed by this container */
                            SizeRw: Option[Long],

                            /** The total size of all the files in this container */
                            SizeRootFs: Option[Long],

                            HostConfig: HostConfig,

                            NetworkSettings: NetworkSettings
                          )

object ContainerStatus {
  implicit val reader: JsonReader[ContainerStatus] = jsonReader[ContainerStatus]
}