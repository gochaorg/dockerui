package xyz.cofe.lima.docker.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class HostConfig(
  // An integer value representing this container's relative CPU weight versus other containers.
  CpuShares: Option[Int],

  // Memory limit in bytes.
  Memory: Option[Long],

  // Path to cgroups under which the container's cgroup is created. If the path is not absolute, the path is considered to be relative to the cgroups path of the init process. Cgroups are created if they do not already exist.
  CgroupParent: Option[String],

  // [ 0 .. 1000 ]
  //Block IO weight (relative weight).
  BlkioWeight: Option[Int],

  // Block IO weight (relative device weight) in the form:
  // [{"Path": "device_path", "Weight": weight}]
  BlkioWeightDevice: Option[List[BlkioWeightDevice]],

  // Limit read rate (bytes per second) from a device, in the form: [{"Path": "device_path", "Rate": rate}]
  BlkioDeviceReadBps: Option[List[ThrottleDevice]],

  // Limit write rate (bytes per second) to a device, in the form: [{"Path": "device_path", "Rate": rate}]
  BlkioDeviceWriteBps: Option[List[ThrottleDevice]],

  // Limit read rate (IO per second) from a device, in the form: [{"Path": "device_path", "Rate": rate}]
  BlkioDeviceReadIOps: Option[List[ThrottleDevice]],

  // Limit write rate (IO per second) to a device, in the form: [{"Path": "device_path", "Rate": rate}]
  BlkioDeviceWriteIOps: Option[List[ThrottleDevice]],

  // The length of a CPU period in microseconds.
  CpuPeriod: Option[Long],

  // Microseconds of CPU time that the container can get in a CPU period.
  CpuQuota: Option[Long],

  // The length of a CPU real-time period in microseconds. Set to 0 to allocate no time allocated to real-time tasks.
  CpuRealtimePeriod: Option[Long],

  // The length of a CPU real-time runtime in microseconds. Set to 0 to allocate no time allocated to real-time tasks.
  CpuRealtimeRuntime: Option[Long],

  // CPUs in which to allow execution (e.g., 0-3, 0,1).
  CpusetCpus: Option[String],

  // Memory nodes (MEMs) in which to allow execution (0-3, 0,1). Only effective on NUMA systems.
  CpusetMems: Option[String],

  // A list of devices to add to the container.
  // Devices : List[Devices],


  // a list of cgroup rules to apply to the container
  DeviceCgroupRules: Option[List[String]],

  // A list of requests for devices to be sent to device drivers.
  // DeviceRequests: List[DeviceRequests],


  // Kernel memory limit in bytes.
  // Deprecated: This field is deprecated as the kernel 5.4 deprecated kmem.limit_in_bytes
  KernelMemory: Option[Long],

  // Hard limit for kernel TCP buffer memory (in bytes).
  KernelMemoryTCP: Option[Long],

  // Memory soft limit in bytes.
  MemoryReservation: Option[Long],

  // Total memory limit (memory + swap). Set as -1 to enable unlimited swap.
  MemorySwap: Option[Long],

  // [ 0 .. 100 ]
  //Tune a container's memory swappiness behavior. Accepts an integer between 0 and 100.
  MemorySwappiness: Option[Int],

  // CPU quota in units of 10-9 CPUs.
  NanoCpus: Option[Long],

  // Disable OOM Killer for the container.
  OomKillDisable: Option[Boolean],

  // Run an init inside the container that forwards signals and reaps processes. This field is omitted if empty, and the default (as configured on the daemon) is used.
  Init: Option[Boolean],

  // Tune a container's PIDs limit. Set 0 or -1 for unlimited, or null to not change.
  PidsLimit: Option[Long],

  // A list of resource limits to set in the container. For example:
  Ulimits: Option[List[Ulimits]],

  // The number of usable CPUs (Windows only).
  //
  //On Windows Server containers, the processor resource controls are mutually exclusive. The order of precedence is CPUCount first, then CPUShares, and CPUPercent last.
  CpuCount: Option[Long],

  // The usable percentage of the available CPUs (Windows only).
  //
  //On Windows Server containers, the processor resource controls are mutually exclusive. The order of precedence is CPUCount first, then CPUShares, and CPUPercent last.
  CpuPercent: Option[Long],

  // Maximum IOps for the container system drive (Windows only)
  IOMaximumIOps: Option[Long],

  // Maximum IO in bytes per second for the container system drive (Windows only).
  IOMaximumBandwidth: Option[Long],

  // A list of volume bindings for this container. Each volume binding is a string in one of these forms:
  //
  //host-src:container-dest[:options] to bind-mount a host path into the container. Both host-src, and container-dest must be an absolute path.
  //volume-name:container-dest[:options] to bind-mount a volume managed by a volume driver into the container. container-dest must be an absolute path.
  //options is an optional, comma-delimited list of:
  //
  //nocopy disables automatic copying of data from the container path to the volume. The nocopy flag only applies to named volumes.
  //[ro|rw] mounts a volume read-only or read-write, respectively. If omitted or set to rw, volumes are mounted read-write.
  //[z|Z] applies SELinux labels to allow or deny multiple containers to read and write to the same volume.
  //z: a shared content label is applied to the content. This label indicates that multiple containers can share the volume content, for both reading and writing.
  //Z: a private unshared label is applied to the content. This label indicates that only the current container can use a private volume. Labeling systems such as SELinux require proper labels to be placed on volume content that is mounted into a container. Without a label, the security system can prevent a container's processes from using the content. By default, the labels set by the host operating system are not modified.
  //[[r]shared|[r]slave|[r]private] specifies mount propagation behavior. This only applies to bind-mounted volumes, not internal volumes or named volumes. Mount propagation requires the source mount point (the location where the source directory is mounted in the host operating system) to have the correct propagation properties. For shared volumes, the source mount point must be set to shared. For slave volumes, the mount must be set to either shared or slave.
  Binds: Option[List[String]],

  // Path to a file where the container ID is written
  ContainerIDFile: Option[String],

  // The logging configuration for this container
  LogConfig: Option[LogConfig],

  // Network mode to use for this container. Supported standard values are: bridge, host, none, and container:<name|id>. Any other value is taken as a custom network's name to which this container should connect to.
  NetworkMode: Option[String],

  // PortMap describes the mapping of container ports to host ports, using the container's port-number and protocol as key in the format <port>/<protocol>, for example, 80/udp.
  //
  //If a container's port is mapped for multiple protocols, separate entries are added to the mapping table.
  PortBindings: Option[Map[String,List[Map[String,String]]]],

  // The behavior to apply when the container exits. The default is not to restart.
  //
  //An ever increasing delay (double the previous delay, starting at 100ms) is added before each restart to prevent flooding the server.
  RestartPolicy: Option[RestartPolicy],


  // Automatically remove the container when the container's process exits. This has no effect if RestartPolicy is set.
  AutoRemove: Option[Boolean],

  // Driver that this container uses to mount volumes.
  VolumeDriver: Option[String],

  // A list of volumes to inherit from another container, specified in the form <container name>[:<ro|rw>].
  VolumesFrom: Option[List[String]],

  // Specification for mounts to be added to the container.
  // Mounts: List[Mounts]


  // A list of kernel capabilities to add to the container. Conflicts with option 'Capabilities'.
  CapAdd: Option[List[String]],


  // A list of kernel capabilities to drop from the container. Conflicts with option 'Capabilities'.
  CapDrop: Option[List[String]],

  // Enum: "private" "host"
  //   cgroup namespace mode for the container. Possible values are:
  //
  //   "private": the container runs in its own private cgroup namespace
  //   "host": use the host system's cgroup namespace
  //   If not specified, the daemon default is used, which can either be "private" or "host", depending on daemon version, kernel support and configuration.
  CgroupnsMode: Option[String],

  // A list of DNS servers for the container to use.
  Dns: Option[List[String]],

  // A list of DNS options.
  DnsOptions: Option[List[String]],

  // A list of DNS search domains.
  DnsSearch: Option[List[String]],

  // A list of hostnames/IP mappings to add to the container's /etc/hosts file. Specified in the form ["hostname:IP"].
  ExtraHosts: Option[List[String]],

  // A list of additional groups that the container process will run as.
  GroupAdd: Option[List[String]],

  // IPC sharing mode for the container. Possible values are:
  //
  //"none": own private IPC namespace, with /dev/shm not mounted
  //"private": own private IPC namespace
  //"shareable": own private IPC namespace, with a possibility to share it with other containers
  //"container:<name|id>": join another (shareable) container's IPC namespace
  //"host": use the host system's IPC namespace
  //If not specified, daemon default is used, which can either be "private" or "shareable", depending on daemon version and configuration.
  IpcMode: Option[String],

  // Cgroup to use for the container.
  Cgroup: Option[String],

  // A list of links for the container in the form container_name:alias.
  Links: Option[List[String]],

  // An integer value containing the score given to the container in order to tune OOM killer preferences.
  OomScoreAdj: Option[Int],

  // Set the PID (Process) Namespace mode for the container. It can be either:
  //
  //"container:<name|id>": joins another container's PID namespace
  //"host": use the host's PID namespace inside the container
  PidMode: Option[String],

  // Gives the container full access to the host.
  Privileged: Option[Boolean],

  // Allocates an ephemeral host port for all of a container's exposed ports.
  //
  //Ports are de-allocated when the container stops and allocated when the container starts. The allocated port might be changed when restarting the container.
  //
  //The port is selected from the ephemeral port range that depends on the kernel. For example, on Linux the range is defined by /proc/sys/net/ipv4/ip_local_port_range.
  PublishAllPorts: Option[Boolean],

  // Mount the container's root filesystem as read only.
  ReadonlyRootfs: Option[Boolean],

  // A list of string values to customize labels for MLS systems, such as SELinux.
  SecurityOpt: Option[List[String]],

  // Storage driver options for this container, in the form {"size": "120G"}.
  StorageOpt: Option[Map[String,String]],

  // A map of container directories which should be replaced by tmpfs mounts, and their corresponding mount options. For example:
  // { "/run": "rw,noexec,nosuid,size=65536k" }
  Tmpfs: Option[Map[String,String]],

  // UTS namespace to use for the container.
  UTSMode: Option[String],

  // Sets the usernamespace mode for the container when usernamespace remapping option is enabled.
  UsernsMode: Option[String],

  // >= 0
  //Size of /dev/shm in bytes. If omitted, the system uses 64MB.
  ShmSize: Option[Int],

  // A list of kernel parameters (sysctls) to set in the container. For example: {"net.ipv4.ip_forward": "1"}
  Sysctls: Option[Map[String,String]],

  // Runtime to use with this container.
  Runtime: Option[String],

  // ConsoleSize -
  //Array of integers = 2 items [ items >= 0 ]
  //Initial console size, as an [height, width] array. (Windows only)

  // Enum: "default" "process" "hyperv"
  // Isolation technology of the container. (Windows only)
  Isolation: Option[String],


  // The list of paths to be masked inside the container (this overrides the default set of paths).
  MaskedPaths: Option[List[String]],

  // The list of paths to be set as read-only inside the container (this overrides the default set of paths).
  ReadonlyPaths: Option[List[String]],
)
object HostConfig {
  implicit val reader: JsonReader[HostConfig] = jsonReader[HostConfig]
  implicit val writer: JsonWriter[HostConfig] = jsonWriter[HostConfig]
}