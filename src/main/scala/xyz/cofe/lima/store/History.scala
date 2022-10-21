package xyz.cofe.lima.store

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}
import tethys._
import tethys.jackson._

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}

trait History[A] {
  def add(item:A):Unit
  def get(index:Int):Option[A]
  def size:Int
}

class HistoryImpl[A:JsonWriter:JsonReader](path:Path,limit:Int) extends History[A] {
  case class Entry[B:JsonWriter:JsonReader](value:B)
  object Entry {
    implicit val entryReader:JsonReader[Entry[A]] = jsonReader[Entry[A]]
    implicit val entryWriter:JsonWriter[Entry[A]] = jsonWriter[Entry[A]]
  }

  private def readFile():Either[String,List[Entry[A]]] = {
    Files.readString(path,StandardCharsets.UTF_8).jsonAs[List[Entry[A]]].left.map(e => e.getMessage)
  }
  private def writeFile(values:List[Entry[A]]) = {
    Files.writeString(path,values.asJson,StandardCharsets.UTF_8)
  }

  private var entries : List[Entry[A]] = readFile().getOrElse(List())

  def add(item:A):Unit = {
    entries = (entries ++ List(Entry(item))).takeRight(limit)
  }

  def get(index:Int):Option[A] = entries.lift(index).map(_.value)

  def size:Int = entries.size
}

object History {
  def apply[A:JsonWriter:JsonReader](path:Path,limit:Int):History[A] =
    new HistoryImpl[A](path, limit)
}
