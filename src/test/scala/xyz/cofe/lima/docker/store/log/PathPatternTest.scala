package xyz.cofe.lima.docker.store.log

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.store.log.PathPattern

import java.nio.file.Path

class PathPatternTest extends AnyFunSuite {
  test("xxx") {
    println(
      PathPattern.parse(Path.of("/a{b}c/{def}/xyz")).generate{ code => Right(code*2) }
    )
    println(
      PathPattern.parse(Path.of("/log/app/{yyyy}-{MM}-{dd}-{hh}-{mm}-{ss}")).generate
    )
    println(
      PathPattern.parse(Path.of("/log/app/{yy}-{MM}-{dd}-{hh}-{mm}-{ss}")).generate
    )
  }
}
