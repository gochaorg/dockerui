package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.{ShowDerivation, TreeShow, TreeShowDerivation, TreeWriter}

class MagnoliaTest extends AnyFunSuite {
  sealed trait Tree[+T]
  case class Branch[+T](left: Tree[T], right: Tree[T]) extends Tree[T]
  case class Leaf[+T](value: T) extends Tree[T]

  val tree = Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))

  import language.experimental.macros, magnolia1._


  test("magnolia test") {
    val show = ShowDerivation.gen[Tree[Int]]
    println( show.show(tree) )
  }

  val containerInspect = model.ContainerInspect(
    "id1", "createdTime", "path", List("a","b"),
    model.ContainerState("status", false, false, false, false, true, 123, 0, "no error", "startedAt","finishedAt", None),
    "image", "resolveConfPath", "hostnamePath", "hostsPath", "logPath", "name", 1, "driver", "platform",
    "mountLabel", "processLabel", "appArmorProfile", None,
    model.HostConfigInspect(4,234,"cgroupParent",1,None,None,None,None,None,
      12345,2345,345,456,"cpusetCups","cpusetMems",None,567,678,789,890,None,123,false,None,None,None,234,345,456,567,None,"containerPidFile",
      model.LogConfig("type",None),"networkMode",Map(),true,"volumeDriver",None,None,None,"cgroupsMode",List(),List(),
      List(),None,None,"ipcMode","cgroup",None,1,"pidMode",false,false,false,None,None,None,"utsMode","usersMode",243,None,"runtime",
      "isolation",List(),List()),None,None,List(),model.ContainerConfig(
      "hostname","domain","user",false,false,false,false,false,false,List(),List(),None,"image","workingDir",
      None,None,None,None,None,None,None
    ),model.NetworkSettingsInspect("bridge","sandboxId",true,"linkLocalIPv6Address",0,"sandboxKey",
      "endpointId","gateway","globalIPv6Address",2,"ipAddress",4,"ipv6gateway","macaddress")
  )

  test("container inspect - show") {
    implicit val _1 = ShowDerivation.gen[model.HostConfigInspect]
    implicit val _2 = ShowDerivation.gen[model.ThrottleDevice]
    val show = ShowDerivation.gen[model.ContainerInspect]
    println(show.show(containerInspect))
  }

  test("container inspect - tree show") {
    val collector = TreeWriter.Writer()

    val showTree = TreeShowDerivation.gen[model.ContainerInspect]
    showTree.show(collector,containerInspect)

    println("roots")
    println(collector.roots)

    println("current")
    println(collector.current)

    println("stack")
    println(collector.stack)
  }
}
