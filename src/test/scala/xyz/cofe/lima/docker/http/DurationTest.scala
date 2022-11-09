package xyz.cofe.lima.docker.http

import org.scalatest.funsuite.AnyFunSuite
import Duration._

class DurationTest extends AnyFunSuite {
  test("toString/parse") {
    assert(
      List(
        1.milliseconds,
        1.seconds,
        1.minutes,
        1.minutes + 2.nanosec,
        1.hours,
        1.days,
        23.seconds,
        120.seconds,
        123.seconds,
        2.days + 3.hours + 4.minutes + 5.seconds + 6.milliseconds,
      ).map { dur =>
        val str= dur.toString
        val prs= Duration.parse(str)
        println(s"str=$str prs=$prs")
        prs.map( dur2 => dur==dur2 ).getOrElse(false)
      }.forall(x => x)
    )
  }
}
