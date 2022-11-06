package xyz.cofe.lima.docker.store.log

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.store.log.PathPattern
import xyz.cofe.lima.store.log.PathPattern.AppHomeProvider

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

  test("app home 1") {
    implicit val home = AppHomeProvider.provide(Path.of("/Users/username/code/project/.dockerui"))
    val pathPattern = PathPattern.parse(Path.of("{appHome}/log/app"))
    println(pathPattern)
    println(pathPattern.generate)

    val succ = pathPattern.predicate match {
      case Left(value) =>
        println(value)
        false
      case Right(pred) =>
        List(
          (Path.of("/Users/username/code/project/.dockerui/log/app"), true),
        ).map { case (path, expect) =>
          val actual = pred(path)
          println(s"path=$path actual=$actual expect=$expect")
          actual == expect
        }.forall( r => r )
    }

    assert(succ)
  }

  test("app home 2") {
    implicit val home = AppHomeProvider.provide(Path.of("/Users/username/code/project/.dockerui"))
    val pathPattern = PathPattern.parse(Path.of("{appHome}/log/app/{yyyy}/{MM}/{dd}"))
    println(pathPattern)
    println(pathPattern.generate)

    val succ = pathPattern.predicate match {
      case Left(value) =>
        println(value)
        false
      case Right(pred) =>
        List(
          (Path.of("/Users/username/code/project/.dockerui/log/app"), false),
          (Path.of("/Users/username/code/project/.dockerui/log/app/1234/01/22"), true),
        ).map { case (path, expect) =>
          val actual = pred(path)
          println(s"path=$path actual=$actual expect=$expect")
          actual == expect
        }.forall( r => r )
    }

    assert(succ)
  }
}
