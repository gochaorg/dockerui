package xyz.cofe.lima.docker.store.log

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.store.log.PathPattern

import java.nio.file.Path

class PathPatternTest extends AnyFunSuite {
  test("generate") {
    println(
      PathPattern.parse(Path.of("/log/app/{yyyy}-{MM}-{dd}-{hh}-{mm}-{ss}")).generate
    )
    println(
      PathPattern.parse(Path.of("/log/app/{yy}-{MM}-{dd}-{hh}-{mm}-{ss}")).generate
    )
  }

  test("pattern") {
    PathPattern.parse(Path.of("/log/app/{yyyy}-{MM}-{dd}-{hh}-{mm}-{ss}")).regex match {
      case Left(value) => println(value)
      case Right(value) => println(value)
    }
  }

  test("predicate test") {
    val succ = PathPattern.parse(Path.of("log/app/{yyyy}/{MM}/{dd}")).predicate match {
      case Left(value) =>
        println(value)
        false
      case Right(pred) =>
        List(
          (Path.of("log/app/1234/01/22"), true),
          (Path.of("log/app/15234/01/22"), false),
          (Path.of("log/app2/1234/01/22"), false)
        ).map { case (path, expect) =>
          val actual = pred(path)
          println(s"path=$path actual=$actual expect=$expect")
          actual == expect
        }.forall( r => r )
    }

    assert(succ)
  }
}
