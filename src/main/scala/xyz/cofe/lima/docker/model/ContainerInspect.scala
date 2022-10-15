package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}
import xyz.cofe.lima.{TreeShow, TreeShowDerivation}

case class ContainerInspect(
                             /** The ID of the container : ba033ac4401106a3b513bc9d639eee123ad78ca3616b921167cd74b20e25ed39 */
                             Id: String,

                             /** The time the container was created : 2015-01-06T15:47:31.485331387Z */
                             Created: String,

                             /** The path to the command being run : /bin/sh */
                             Path: String,

                             /** The arguments to the command being run */
                             Args: List[String],

                             /** ContainerState stores container's running state. It's part of ContainerJSONBase and will be returned by the "inspect" command. */
                             State: ContainerState,

                             /** The container's image ID */
                             Image: String,

                             ResolvConfPath: String,
                             HostnamePath: String,
                             HostsPath: String,
                             LogPath: String,
                             Name: String,
                             RestartCount: Int,
                             Driver: String,
                             Platform: String,
                             MountLabel: String,
                             ProcessLabel: String,
                             AppArmorProfile: String,
                             ExecIDs: Option[List[String]],
                             HostConfig: HostConfigInspect,
                             //GraphDriver:
                             SizeRw: Option[Long],
                             SizeRootFs: Option[Long],
                             Mounts: List[Mounts],
                             Config: ContainerConfig,
                             NetworkSettings: NetworkSettingsInspect
                           )
object ContainerInspect {
  implicit val reader: JsonReader[ContainerInspect] = jsonReader[ContainerInspect]
  implicit val writer: JsonWriter[ContainerInspect] = jsonWriter[ContainerInspect]
  implicit val showTree: TreeShow[ContainerInspect] = TreeShowDerivation.gen[ContainerInspect]
}
