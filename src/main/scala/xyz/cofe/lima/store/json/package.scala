package xyz.cofe.lima.store

import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator
import tethys.writers.tokens.TokenWriter
import tethys.{JsonObjectWriter, JsonReader, JsonWriter}

import java.time.{Instant, LocalDateTime, ZoneId, ZoneOffset}
import java.time.format.DateTimeFormatter
import scala.reflect.ClassTag

package object json {
  def classWriter[A](implicit ct: ClassTag[A]): JsonObjectWriter[A] = {
    JsonWriter.obj[A].addField("_type")(_ => ct.runtimeClass.getSimpleName)
  }

  trait DateTimeFormatterProvide {
    def dateTimeFormatter: DateTimeFormatter
  }

  object DateTimeFormatterProvide {
    implicit val defaultFormatter: DateTimeFormatterProvide = new DateTimeFormatterProvide {
      lazy val dateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    }
  }

  implicit def instantJsonWriter(implicit dtFormatProvider: DateTimeFormatterProvide):JsonWriter[Instant] = new JsonWriter[Instant] {
    override def write(value: Instant, tokenWriter: TokenWriter): Unit = {
      val ltime = value.atZone(ZoneId.systemDefault())
      val df = dtFormatProvider.dateTimeFormatter
      tokenWriter.writeString(
        df.format(ltime)
      )
    }
  }
  implicit def instantJsonReader(implicit dtFormatProvider: DateTimeFormatterProvide):JsonReader[Instant] = {
    val strReader = implicitly[JsonReader[String]]
    val df = dtFormatProvider.dateTimeFormatter
    strReader.map { str =>
      val ltime = LocalDateTime.parse(str,df)
      ltime.toInstant(ZoneOffset.UTC)
    }
  }

  implicit def localDateTimeJsonWriter(implicit dtFormatProvider: DateTimeFormatterProvide):JsonWriter[LocalDateTime] = new JsonWriter[LocalDateTime] {
    override def write(value: LocalDateTime, tokenWriter: TokenWriter): Unit = {
      val df = dtFormatProvider.dateTimeFormatter
      tokenWriter.writeString(
        df.format(value)
      )
    }
  }
  implicit def localDateTimeJsonReader(implicit dtFormatProvider: DateTimeFormatterProvide):JsonReader[LocalDateTime] = {
    val strReader = implicitly[JsonReader[String]]
    val df = dtFormatProvider.dateTimeFormatter
    strReader.map { str =>
      LocalDateTime.parse(str, df)
    }
  }
}
