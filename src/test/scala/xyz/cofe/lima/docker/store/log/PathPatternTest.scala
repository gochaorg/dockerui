package xyz.cofe.lima.docker.store.log

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.store.log.PathPattern

import java.nio.file.Path

class PathPatternTest extends AnyFunSuite {
  test("xxx") {
    println(
      PathPattern.parse(Path.of("/a{b}c/{def}/xyz")).generate{ code => Right(code*2) }
    )
  }
}
