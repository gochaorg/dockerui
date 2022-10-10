package xyz.cofe.lima.docker

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

object model {
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

  case class NetworkSettings(
    Networks: Map[String,Network]
  )
  object NetworkSettings {
    implicit val reader: JsonReader[NetworkSettings] = jsonReader[NetworkSettings]
  }

  case class Network(
    IPAMConfig: Option[IPAMConfig],
    Links: Option[List[String]],
    Aliases: Option[List[String]],
    NetworkID: String,
    EndpointID: Option[String],
    Gateway: Option[String],
    IPAddress: Option[String],
    IPPrefixLen: Option[Int],
    IPv6Gateway: Option[String],
    GlobalIPv6Address: Option[String],
    GlobalIPv6PrefixLen: Option[Long],
    MacAddress:String,
  )
  object Network {
    implicit val reader: JsonReader[Network] = jsonReader[Network]
  }

  case class IPAMConfig(
    IPv4Address: String,
    IPv6Address: String,
    LinkLocalIPs: List[String]
  )
  object IPAMConfig {
    implicit val reader: JsonReader[IPAMConfig] = jsonReader[IPAMConfig]
  }

  case class HostConfig(
    NetworkMode: String
  )
  object HostConfig {
    implicit val reader: JsonReader[HostConfig] = jsonReader[HostConfig]
  }

  case class PortMap(
    IP: Option[String],
    PrivatePort: Option[Int],
    PublicPort: Option[Int],
    Type: String
  )
  object PortMap {
    implicit val reader: JsonReader[PortMap] = jsonReader[PortMap]
  }

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
    implicit val showTree: TreeShow[ContainerInspect] = TreeShowDerivation.gen[ContainerInspect]
  }

  case class Mounts(
                     Name: String,
                     Source: String,
                     Destination: String,
                     Driver: String,
                     Mode: String,
                     RW: Boolean,
                     Propagation: String,
                   )
  object Mounts {
    implicit val reader: JsonReader[Mounts] = jsonReader[Mounts]
  }

  case class NetworkSettingsInspect(
                                     Bridge: String,
                                     SandboxID: String,
                                     HairpinMode: Boolean,
                                     LinkLocalIPv6Address: String,
                                     LinkLocalIPv6PrefixLen: Int,
                                     //Ports
                                     SandboxKey: String,
                                     // SecondaryIPAddresses : Array of objects or null (Address)
                                     // SecondaryIPv6Addresses : Array of objects or null (Address)
                                     EndpointID: String,
                                     Gateway: String,
                                     GlobalIPv6Address: String,
                                     GlobalIPv6PrefixLen: Int,
                                     IPAddress: String,
                                     IPPrefixLen: Int,
                                     IPv6Gateway: String,
                                     MacAddress: String,
                                     // Networks:
                                   )
  object NetworkSettingsInspect {
    implicit val reader: JsonReader[NetworkSettingsInspect] = jsonReader[NetworkSettingsInspect]
  }

  case class ContainerConfig(
    Hostname: String,
    Domainname: String,
    User: String,
    AttachStdin: Boolean,
    AttachStdout: Boolean,
    AttachStderr: Boolean,
    // ExposedPorts
    Tty: Boolean,
    OpenStdin: Boolean,
    StdinOnce: Boolean,
    Env: List[String],
    Cmd: List[String],
    // Healthcheck
    ArgsEscaped: Option[Boolean],
    Image: String,
    // Volumes
    WorkingDir: String,
    Entrypoint: Option[List[String]],
    NetworkDisabled: Option[Boolean],
    MacAddress: Option[String],
    OnBuild: Option[List[String]],
    // Labels:
    StopSignal: Option[String],
    StopTimeout: Option[String],
    Shell: Option[List[String]],
  )
  object ContainerConfig {
    implicit val reader: JsonReader[ContainerConfig] = jsonReader[ContainerConfig]
  }

  case class HostConfigInspect(
      CpuShares: Int,
      Memory: Long,
      CgroupParent: String,
      BlkioWeight: Int,
      BlkioWeightDevice: Option[List[BlkioWeightDevice]],
      BlkioDeviceReadBps: Option[List[ThrottleDevice]],
      BlkioDeviceWriteBps: Option[List[ThrottleDevice]],
      BlkioDeviceReadIOps: Option[List[ThrottleDevice]],
      BlkioDeviceWriteIOps: Option[List[ThrottleDevice]],
      CpuPeriod: Long,
      CpuQuota: Long,
      CpuRealtimePeriod: Long,
      CpuRealtimeRuntime: Long,
      CpusetCpus: String,
      CpusetMems: String,
      // Devices : List[Devices],
      DeviceCgroupRules: Option[List[String]],
      // DeviceRequests: List[DeviceRequests],
      KernelMemory: Long,
      KernelMemoryTCP: Long,
      MemoryReservation: Long,
      MemorySwap: Long,
      MemorySwappiness: Option[Int],
      NanoCpus: Long,
      OomKillDisable: Boolean,
      Init: Option[Boolean],
      PidsLimit: Option[Long],
      Ulimits: Option[List[Ulimits]],
      CpuCount: Long,
      CpuPercent: Long,
      IOMaximumIOps: Long,
      IOMaximumBandwidth: Long,
      Binds: Option[List[String]],
      ContainerIDFile: String,
      LogConfig: LogConfig,
      NetworkMode: String,
      PortBindings: Map[String,List[Map[String,String]]],
      // RestartPolicy:
      AutoRemove: Boolean,
      VolumeDriver: String,
      VolumesFrom: Option[List[String]],
      // Mounts: List[Mounts]
      CapAdd: Option[List[String]],
      CapDrop: Option[List[String]],
      CgroupnsMode: String,
      Dns: List[String],
      DnsOptions: List[String],
      DnsSearch: List[String],
      ExtraHosts: Option[List[String]],
      GroupAdd: Option[List[String]],
      IpcMode: String,
      Cgroup: String,
      Links: Option[List[String]],
      OomScoreAdj: Int,
      PidMode: String,
      Privileged: Boolean,
      PublishAllPorts: Boolean,
      ReadonlyRootfs: Boolean,
      SecurityOpt: Option[List[String]],
      StorageOpt: Option[Map[String,String]],
      Tmpfs: Option[Map[String,String]],
      UTSMode: String,
      UsernsMode: String,
      ShmSize: Int,
      Sysctls: Option[Map[String,String]],
      Runtime: String,

      // ConsoleSize -
      //Array of integers = 2 items [ items >= 0 ]
      //Initial console size, as an [height, width] array. (Windows only)

      // "default" "process" "hyperv"
      Isolation: String,

      MaskedPaths: List[String],
      ReadonlyPaths: List[String],
  )
  object HostConfigInspect {
    implicit val reader: JsonReader[HostConfigInspect] = jsonReader[HostConfigInspect]
  }

  case class LogConfig(
    Type: String,
    Config: Option[Map[String,String]],
  )
  object LogConfig {
    implicit val reader: JsonReader[LogConfig] = jsonReader[LogConfig]
  }

  case class Ulimits(
      Name: String,
      Soft: Int,
      Hard: Int,
  )
  object Ulimits {
    implicit val reader: JsonReader[Ulimits] = jsonReader[Ulimits]
  }

  case class BlkioWeightDevice(
      Path: String,
      Weight: Int,
  )
  object BlkioWeightDevice {
    implicit val reader: JsonReader[BlkioWeightDevice] = jsonReader[BlkioWeightDevice]
  }

  case class ThrottleDevice(
      Path: String,
      Rate: Double
  )
  object ThrottleDevice{
    implicit val reader: JsonReader[ThrottleDevice] = jsonReader[ThrottleDevice]
  }

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
  }

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

  case class Top(
                  Titles: List[String],
                  Processes: List[List[String]]
                )
  object Top {
    implicit val reader: JsonReader[Top] = jsonReader[Top]
  }


}
