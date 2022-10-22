package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.docker.log.Logger
import tethys._
import tethys.jackson._

class DockerLogTest extends AnyFunSuite {
  test("log event") {
    val ev = Logger.ImageCreate()
    println(ev.asJson)
  }
}
