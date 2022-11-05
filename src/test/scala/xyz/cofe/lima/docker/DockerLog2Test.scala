package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.log.Logger.{Containers, LogEvent}
import xyz.cofe.lima.store.json._

class DockerLog2Test extends AnyFunSuite {
  val logEntry =
    """{"_type":"SuccEvent",
      |  "threadId":{"id":18,"name":"JavaFX Application Thread"},
      |  "beginTime":"2022-10-31T04:01:21.057027419",
      |  "endTime":"2022-10-31T04:01:21.141861409",
      |  "args":{
      |    "_type":"Containers","all":true,"size":false
      |  },
      |
      |  "result":[
      |    {
      |      "Id":"53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4",
      |      "Names":["/n1"],
      |      "Image":"alpine",
      |      "ImageID":"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5",
      |      "Command":"echo test",
      |      "Created":1666480250,
      |      "Ports":[],
      |      "Labels":{},
      |      "State":"exited","Status":"Exited (0) 8 days ago",
      |      "HostConfig":{"NetworkMode":"default"},
      |      "NetworkSettings":
      |      {
      |        "Networks":{
      |          "bridge":{
      |            "NetworkID":"89e112550a285c17d70fea06881feb46212aa788180181344e0d95158b88b10f",
      |            "EndpointID":"",
      |            "Gateway":"",
      |            "IPAddress":"",
      |            "IPPrefixLen":0,
      |            "IPv6Gateway":"",
      |            "GlobalIPv6Address":"",
      |            "GlobalIPv6PrefixLen":0,
      |            "MacAddress":""
      |           }
      |         }
      |       }
      |     },
      |     {
      |       "Id":"1668f9b53221b7cc09beb97c162a23ead66d11a1fd0127bcb10228869d55da79",
      |       "Names":["/ecstatic_kalam"],
      |       "Image":"alpine",
      |       "ImageID":"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5",
      |       "Command":"echo hello",
      |       "Created":1666478584,
      |       "Ports":[],
      |       "Labels":{},
      |       "State":"exited",
      |       "Status":"Exited (0) 8 days ago",
      |       "HostConfig":{
      |         "NetworkMode":"default"
      |       },
      |       "NetworkSettings":{
      |         "Networks":{
      |           "bridge":{
      |             "NetworkID":"89e112550a285c17d70fea06881feb46212aa788180181344e0d95158b88b10f",
      |             "EndpointID":"",
      |             "Gateway":"",
      |             "IPAddress":"",
      |             "IPPrefixLen":0,
      |             "IPv6Gateway":"",
      |             "GlobalIPv6Address":"",
      |             "GlobalIPv6PrefixLen":0,
      |             "MacAddress":""
      |           }
      |         }
      |       }
      |     }
      |   ]
      |}
      |""".stripMargin

  test("test read log entry") {
    import xyz.cofe.lima.store.json.Query._
    import xyz.cofe.lima.store.json.TethysToks._

    val tree = logEntry.jsonAs[JsValue]
    println(tree.query("_type").string)
    println(tree.query("args")("_type").string)

    println(tree.query("args").jsObject.map(_.tokens))
    tree.query("args").jsObject.map(_.jsonAs[Containers]).get match {
      case Left(value) => println(value)
      case Right(value) => println(value)
    }

    tree.query("result").jsValue.map(_.jsonAs[Containers#RESULT]) match {
      case Some(value) => value match {
        case Left(value) => ???
        case Right(value) => println(value)
      }
      case None => ???
    }
  }

  val logEntry2 = """{
                    |  "_type": "FailEvent",
                    |  "threadId": {
                    |    "id": 1,
                    |    "name": "ScalaTest-run-running-DockerTest"
                    |  },
                    |  "beginTime": "2022-11-06T00:31:47.151826108",
                    |  "endTime": "2022-11-06T00:31:47.230523849",
                    |  "args": {
                    |    "_type": "ContainerInspect",
                    |    "id": "53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4"
                    |  },
                    |  "error": {
                    |    "bodyText": "{\"Id\":\"53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4\",\"Created\":\"2022-10-22T23:10:50.661580264Z\",\"Path\":\"echo\",\"Args\":[\"test\"],\"State\":{\"Status\":\"exited\",\"Running\":false,\"Paused\":false,\"Restarting\":false,\"OOMKilled\":false,\"Dead\":false,\"Pid\":0,\"ExitCode\":0,\"Error\":\"\",\"StartedAt\":\"2022-11-04T23:33:16.344062846Z\",\"FinishedAt\":\"2022-11-04T23:33:16.343965534Z\"},\"Image\":\"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5\",\"ResolvConfPath\":\"/var/lib/docker/containers/53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4/resolv.conf\",\"HostnamePath\":\"/var/lib/docker/containers/53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4/hostname\",\"HostsPath\":\"/var/lib/docker/containers/53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4/hosts\",\"LogPath\":\"/var/lib/docker/containers/53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4/53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4-json.log\",\"Name\":\"/n1\",\"RestartCount\":0,\"Driver\":\"overlay2\",\"Platform\":\"linux\",\"MountLabel\":\"\",\"ProcessLabel\":\"\",\"AppArmorProfile\":\"docker-default\",\"ExecIDs\":null,\"HostConfig\":{\"Binds\":[\"/home/user/.swt:/mnt-b\"],\"ContainerIDFile\":\"\",\"LogConfig\":{\"Type\":\"json-file\",\"Config\":{}},\"NetworkMode\":\"default\",\"PortBindings\":{},\"RestartPolicy\":{\"Name\":\"no\",\"MaximumRetryCount\":0},\"AutoRemove\":false,\"VolumeDriver\":\"\",\"VolumesFrom\":null,\"CapAdd\":null,\"CapDrop\":null,\"CgroupnsMode\":\"private\",\"Dns\":[],\"DnsOptions\":[],\"DnsSearch\":[],\"ExtraHosts\":null,\"GroupAdd\":null,\"IpcMode\":\"private\",\"Cgroup\":\"\",\"Links\":null,\"OomScoreAdj\":0,\"PidMode\":\"\",\"Privileged\":false,\"PublishAllPorts\":false,\"ReadonlyRootfs\":false,\"SecurityOpt\":null,\"UTSMode\":\"\",\"UsernsMode\":\"\",\"ShmSize\":67108864,\"Runtime\":\"runc\",\"ConsoleSize\":[0,0],\"Isolation\":\"\",\"CpuShares\":0,\"Memory\":0,\"NanoCpus\":0,\"CgroupParent\":\"\",\"BlkioWeight\":0,\"BlkioWeightDevice\":[],\"BlkioDeviceReadBps\":null,\"BlkioDeviceWriteBps\":null,\"BlkioDeviceReadIOps\":null,\"BlkioDeviceWriteIOps\":null,\"CpuPeriod\":0,\"CpuQuota\":0,\"CpuRealtimePeriod\":0,\"CpuRealtimeRuntime\":0,\"CpusetCpus\":\"\",\"CpusetMems\":\"\",\"Devices\":[],\"DeviceCgroupRules\":null,\"DeviceRequests\":null,\"KernelMemory\":0,\"KernelMemoryTCP\":0,\"MemoryReservation\":0,\"MemorySwap\":0,\"MemorySwappiness\":null,\"OomKillDisable\":null,\"PidsLimit\":null,\"Ulimits\":null,\"CpuCount\":0,\"CpuPercent\":0,\"IOMaximumIOps\":0,\"IOMaximumBandwidth\":0,\"MaskedPaths\":[\"/proc/asound\",\"/proc/acpi\",\"/proc/kcore\",\"/proc/keys\",\"/proc/latency_stats\",\"/proc/timer_list\",\"/proc/timer_stats\",\"/proc/sched_debug\",\"/proc/scsi\",\"/sys/firmware\"],\"ReadonlyPaths\":[\"/proc/bus\",\"/proc/fs\",\"/proc/irq\",\"/proc/sys\",\"/proc/sysrq-trigger\"]},\"GraphDriver\":{\"Data\":{\"LowerDir\":\"/var/lib/docker/overlay2/5d8a478ba31fae14c8beabfacfae42523a33e205487832527476ab1926b11809-init/diff:/var/lib/docker/overlay2/2a433c1fcf8c59e994111ace833c09baad2745a9a6a11a3677688f44b1283b05/diff\",\"MergedDir\":\"/var/lib/docker/overlay2/5d8a478ba31fae14c8beabfacfae42523a33e205487832527476ab1926b11809/merged\",\"UpperDir\":\"/var/lib/docker/overlay2/5d8a478ba31fae14c8beabfacfae42523a33e205487832527476ab1926b11809/diff\",\"WorkDir\":\"/var/lib/docker/overlay2/5d8a478ba31fae14c8beabfacfae42523a33e205487832527476ab1926b11809/work\"},\"Name\":\"overlay2\"},\"Mounts\":[{\"Type\":\"bind\",\"Source\":\"/home/user/.swt\",\"Destination\":\"/mnt-b\",\"Mode\":\"\",\"RW\":true,\"Propagation\":\"rprivate\"}],\"Config\":{\"Hostname\":\"53e9e78d58ef\",\"Domainname\":\"\",\"User\":\"\",\"AttachStdin\":false,\"AttachStdout\":true,\"AttachStderr\":true,\"Tty\":false,\"OpenStdin\":false,\"StdinOnce\":false,\"Env\":[\"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin\"],\"Cmd\":[\"echo\",\"test\"],\"Image\":\"alpine\",\"Volumes\":null,\"WorkingDir\":\"\",\"Entrypoint\":null,\"OnBuild\":null,\"Labels\":{}},\"NetworkSettings\":{\"Bridge\":\"\",\"SandboxID\":\"3c7b056a4d68fa6e8225b8e322c5f3f931468330a4585bee70e32f96ac399e3e\",\"HairpinMode\":false,\"LinkLocalIPv6Address\":\"\",\"LinkLocalIPv6PrefixLen\":0,\"Ports\":{},\"SandboxKey\":\"/var/run/docker/netns/3c7b056a4d68\",\"SecondaryIPAddresses\":null,\"SecondaryIPv6Addresses\":null,\"EndpointID\":\"\",\"Gateway\":\"\",\"GlobalIPv6Address\":\"\",\"GlobalIPv6PrefixLen\":0,\"IPAddress\":\"\",\"IPPrefixLen\":0,\"IPv6Gateway\":\"\",\"MacAddress\":\"\",\"Networks\":{\"bridge\":{\"IPAMConfig\":null,\"Links\":null,\"Aliases\":null,\"NetworkID\":\"4a955ecc7b049d87122c26ae1e2aba68a380876cbdf022ebc1f57d52f7a4323c\",\"EndpointID\":\"\",\"Gateway\":\"\",\"IPAddress\":\"\",\"IPPrefixLen\":0,\"IPv6Gateway\":\"\",\"GlobalIPv6Address\":\"\",\"GlobalIPv6PrefixLen\":0,\"MacAddress\":\"\",\"DriverOpts\":null}}}}\n",
                    |    "className": "public class xyz.cofe.lima.docker.model.ContainerInspect",
                    |    "jsonParseErr": {
                    |      "exceptionClassName": "tethys.readers.ReaderError",
                    |      "message": "Illegal json at '[ROOT].Mounts[0]': Can not extract fields from json'Name', 'Driver'",
                    |      "localizedMessage": "Illegal json at '[ROOT].Mounts[0]': Can not extract fields from json'Name', 'Driver'",
                    |      "stackTrace": [
                    |        {
                    |          "className": "tethys.readers.ReaderError$",
                    |          "fileName": "ReaderError.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 13,
                    |          "methodName": "wrongJson",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.model.Mounts$$anon$1",
                    |          "fileName": "Mounts.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 16,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.model.Mounts$$anon$1",
                    |          "fileName": "Mounts.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 16,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityIterableReaders$$anon$7",
                    |          "fileName": "IterableReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 63,
                    |          "methodName": "appendBuilder",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityIterableReaders$TraversableReader",
                    |          "fileName": "IterableReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 85,
                    |          "methodName": "recRead",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityIterableReaders$TraversableReader",
                    |          "fileName": "IterableReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 72,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityIterableReaders$TraversableReader",
                    |          "fileName": "IterableReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 67,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityOptionReaders$$anon$7",
                    |          "fileName": "OptionReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 48,
                    |          "methodName": "readSomeValue",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityOptionReaders$OptionJsonReader",
                    |          "fileName": "OptionReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 62,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.instances.LowPriorityOptionReaders$OptionJsonReader",
                    |          "fileName": "OptionReaders.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 53,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.model.ContainerInspect$$anon$1",
                    |          "fileName": "ContainerInspect.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 47,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.model.ContainerInspect$$anon$1",
                    |          "fileName": "ContainerInspect.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 47,
                    |          "methodName": "read",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.package$TokenIteratorOps$",
                    |          "fileName": "package.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 63,
                    |          "methodName": "$anonfun$readJson$extension$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.readers.ReaderError$",
                    |          "fileName": "ReaderError.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 18,
                    |          "methodName": "catchNonFatal",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.package$TokenIteratorOps$",
                    |          "fileName": "package.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 63,
                    |          "methodName": "readJson$extension",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.package$ReaderReaderOps$",
                    |          "fileName": "package.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 48,
                    |          "methodName": "$anonfun$readJson$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "scala.util.Either$RightProjection",
                    |          "fileName": "Either.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 757,
                    |          "methodName": "flatMap",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.package$ReaderReaderOps$",
                    |          "fileName": "package.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 48,
                    |          "methodName": "readJson$extension",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "tethys.package$StringReaderOps$",
                    |          "fileName": "package.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 37,
                    |          "methodName": "jsonAs$extension",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.DockerClient$Expectation",
                    |          "fileName": "DockerClient.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 112,
                    |          "methodName": "json",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.DockerClient",
                    |          "fileName": "DockerClient.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 162,
                    |          "methodName": "containerInspect",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "xyz.cofe.lima.docker.DockerTest",
                    |          "fileName": "DockerTest.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 54,
                    |          "methodName": "$anonfun$new$5",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.OutcomeOf",
                    |          "fileName": "OutcomeOf.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 85,
                    |          "methodName": "outcomeOf",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.OutcomeOf",
                    |          "fileName": "OutcomeOf.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 83,
                    |          "methodName": "outcomeOf$",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.OutcomeOf$",
                    |          "fileName": "OutcomeOf.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 104,
                    |          "methodName": "outcomeOf",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.Transformer",
                    |          "fileName": "Transformer.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 22,
                    |          "methodName": "apply",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.Transformer",
                    |          "fileName": "Transformer.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 20,
                    |          "methodName": "apply",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike$$anon$1",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 226,
                    |          "methodName": "apply",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.TestSuite",
                    |          "fileName": "TestSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 196,
                    |          "methodName": "withFixture",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.TestSuite",
                    |          "fileName": "TestSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 195,
                    |          "methodName": "withFixture$",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuite",
                    |          "fileName": "AnyFunSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1564,
                    |          "methodName": "withFixture",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 224,
                    |          "methodName": "invokeWithFixture$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 236,
                    |          "methodName": "$anonfun$runTest$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.SuperEngine",
                    |          "fileName": "Engine.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 306,
                    |          "methodName": "runTestImpl",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 236,
                    |          "methodName": "runTest",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 218,
                    |          "methodName": "runTest$",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuite",
                    |          "fileName": "AnyFunSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1564,
                    |          "methodName": "runTest",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 269,
                    |          "methodName": "$anonfun$runTests$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.SuperEngine",
                    |          "fileName": "Engine.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 413,
                    |          "methodName": "$anonfun$runTestsInBranch$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "scala.collection.immutable.List",
                    |          "fileName": "List.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 333,
                    |          "methodName": "foreach",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.SuperEngine",
                    |          "fileName": "Engine.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 401,
                    |          "methodName": "traverseSubNodes$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.SuperEngine",
                    |          "fileName": "Engine.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 396,
                    |          "methodName": "runTestsInBranch",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.SuperEngine",
                    |          "fileName": "Engine.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 475,
                    |          "methodName": "runTestsImpl",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 269,
                    |          "methodName": "runTests",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 268,
                    |          "methodName": "runTests$",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuite",
                    |          "fileName": "AnyFunSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1564,
                    |          "methodName": "runTests",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.Suite",
                    |          "fileName": "Suite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1114,
                    |          "methodName": "run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.Suite",
                    |          "fileName": "Suite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1096,
                    |          "methodName": "run$",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuite",
                    |          "fileName": "AnyFunSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1564,
                    |          "methodName": "org$scalatest$funsuite$AnyFunSuiteLike$$super$run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 273,
                    |          "methodName": "$anonfun$run$1",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.SuperEngine",
                    |          "fileName": "Engine.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 535,
                    |          "methodName": "runImpl",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 273,
                    |          "methodName": "run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuiteLike",
                    |          "fileName": "AnyFunSuiteLike.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 272,
                    |          "methodName": "run$",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.funsuite.AnyFunSuite",
                    |          "fileName": "AnyFunSuite.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1564,
                    |          "methodName": "run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.SuiteRunner",
                    |          "fileName": "SuiteRunner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 47,
                    |          "methodName": "run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1321,
                    |          "methodName": "$anonfun$doRunRunRunDaDoRunRun$13",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1315,
                    |          "methodName": "$anonfun$doRunRunRunDaDoRunRun$13$adapted",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "scala.collection.immutable.List",
                    |          "fileName": "List.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 333,
                    |          "methodName": "foreach",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1315,
                    |          "methodName": "doRunRunRunDaDoRunRun",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 992,
                    |          "methodName": "$anonfun$runOptionallyWithPassFailReporter$24",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 970,
                    |          "methodName": "$anonfun$runOptionallyWithPassFailReporter$24$adapted",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 1481,
                    |          "methodName": "withClassLoaderAndDispatchReporter",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 970,
                    |          "methodName": "runOptionallyWithPassFailReporter",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner$",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 798,
                    |          "methodName": "run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.scalatest.tools.Runner",
                    |          "fileName": "Runner.scala",
                    |          "classLoaderName": "app",
                    |          "lineNumber": -1,
                    |          "methodName": "run",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner",
                    |          "fileName": "ScalaTestRunner.java",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 43,
                    |          "methodName": "runScalaTest2or3",
                    |          "nativeMethod": false
                    |        },
                    |        {
                    |          "className": "org.jetbrains.plugins.scala.testingSupport.scalaTest.ScalaTestRunner",
                    |          "fileName": "ScalaTestRunner.java",
                    |          "classLoaderName": "app",
                    |          "lineNumber": 26,
                    |          "methodName": "main",
                    |          "nativeMethod": false
                    |        }
                    |      ],
                    |      "suppressed": []
                    |    },
                    |    "_type": "CantExtractJson"
                    |  }
                    |}""".stripMargin
}
