package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class HostConfigInspect(
                              CpuShares: Option[Int],
                              Memory: Option[Long],
                              CgroupParent: Option[String],
                              BlkioWeight: Option[Int],
                              BlkioWeightDevice: Option[List[BlkioWeightDevice]],
                              BlkioDeviceReadBps: Option[List[ThrottleDevice]],
                              BlkioDeviceWriteBps: Option[List[ThrottleDevice]],
                              BlkioDeviceReadIOps: Option[List[ThrottleDevice]],
                              BlkioDeviceWriteIOps: Option[List[ThrottleDevice]],
                              CpuPeriod: Option[Long],
                              CpuQuota: Option[Long],
                              CpuRealtimePeriod: Option[Long],
                              CpuRealtimeRuntime: Option[Long],
                              CpusetCpus: Option[String],
                              CpusetMems: Option[String],
                              // Devices : List[Devices],
                              DeviceCgroupRules: Option[List[String]],
                              // DeviceRequests: List[DeviceRequests],
                              KernelMemory: Option[Long],
                              KernelMemoryTCP: Option[Long],
                              MemoryReservation: Option[Long],
                              MemorySwap: Option[Long],
                              MemorySwappiness: Option[Int],
                              NanoCpus: Option[Long],
                              OomKillDisable: Option[Boolean],
                              Init: Option[Boolean],
                              PidsLimit: Option[Long],
                              Ulimits: Option[List[Ulimits]],
                              CpuCount: Option[Long],
                              CpuPercent: Option[Long],
                              IOMaximumIOps: Option[Long],
                              IOMaximumBandwidth: Option[Long],
                              Binds: Option[List[String]],
                              ContainerIDFile: Option[String],
                              LogConfig: Option[LogConfig],
                              NetworkMode: Option[String],
                              PortBindings: Option[Map[String,List[Map[String,String]]]],
                              // RestartPolicy:
                              AutoRemove: Option[Boolean],
                              VolumeDriver: Option[String],
                              VolumesFrom: Option[List[String]],
                              // Mounts: List[Mounts]
                              CapAdd: Option[List[String]],
                              CapDrop: Option[List[String]],
                              CgroupnsMode: Option[String],
                              Dns: Option[List[String]],
                              DnsOptions: Option[List[String]],
                              DnsSearch: Option[List[String]],
                              ExtraHosts: Option[List[String]],
                              GroupAdd: Option[List[String]],
                              IpcMode: Option[String],
                              Cgroup: Option[String],
                              Links: Option[List[String]],
                              OomScoreAdj: Option[Int],
                              PidMode: Option[String],
                              Privileged: Option[Boolean],
                              PublishAllPorts: Option[Boolean],
                              ReadonlyRootfs: Option[Boolean],
                              SecurityOpt: Option[List[String]],
                              StorageOpt: Option[Map[String,String]],
                              Tmpfs: Option[Map[String,String]],
                              UTSMode: Option[String],
                              UsernsMode: Option[String],
                              ShmSize: Option[Int],
                              Sysctls: Option[Map[String,String]],
                              Runtime: Option[String],

                              // ConsoleSize -
                              //Array of integers = 2 items [ items >= 0 ]
                              //Initial console size, as an [height, width] array. (Windows only)

                              // "default" "process" "hyperv"
                              Isolation: Option[String],

                              MaskedPaths: Option[List[String]],
                              ReadonlyPaths: Option[List[String]],
                            )
object HostConfigInspect {
  implicit val reader: JsonReader[HostConfigInspect] = jsonReader[HostConfigInspect]
  implicit val writer: JsonWriter[HostConfigInspect] = jsonWriter[HostConfigInspect]
}