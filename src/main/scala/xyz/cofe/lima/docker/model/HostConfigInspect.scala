package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

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