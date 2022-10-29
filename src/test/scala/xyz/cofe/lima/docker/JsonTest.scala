package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDateTime, ZoneId}
import tethys._
import tethys.jackson._
import tethys.writers.tokens.TokenWriter

class JsonTest extends AnyFunSuite {
  test("time 2 json") {
    val time = Instant.now()
    val localtime = time.atZone(ZoneId.systemDefault())
    val dateTimeFormat = DateTimeFormatter.ISO_DATE_TIME
    println( dateTimeFormat.format(localtime) )

    implicit val json = new JsonWriter[Instant] {
      override def write(value: Instant, tokenWriter: TokenWriter): Unit = {
        val ltime = value.atZone(ZoneId.systemDefault())
        val df = DateTimeFormatter.ISO_LOCAL_DATE_TIME
        tokenWriter.writeString(df.format(ltime))
      }
    }

    println(time.asJson)
  }

  test("json 2 time") {
    val df = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    val sample = "2022-10-29T23:39:34.322446653"
    println(LocalDateTime.parse(sample,df))
  }
}
