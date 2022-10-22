package xyz.cofe.lima.store

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}
import tethys._
import tethys.jackson._
import xyz.cofe.lima.fs.JavaNioTracer

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path}
import xyz.cofe.lima.fs.syntax._

trait History[A] {
  def add(item:A):Unit
  def get(index:Int):Option[A]
  def size:Int
  def last:Option[A] = {
    if(size<1)
      None
    else
      get(size-1)
  }
}

class HistoryImpl[A:JsonWriter:JsonReader]
  (path:Path,limit:Int)
  (implicit trace: JavaNioTracer)
extends History[A]
{

  case class Entry[B:JsonWriter:JsonReader](value:B)
  object Entry {
    implicit val entryReader:JsonReader[Entry[A]] = jsonReader[Entry[A]]
    implicit val entryWriter:JsonWriter[Entry[A]] = jsonWriter[Entry[A]]
  }

  private def readFile():Either[String,List[Entry[A]]] = {
    for {
      exists <- path.isFile.left.map(err => err.getMessage)
      entries <- if (exists) {
        path.json.read[List[Entry[A]]].left.map(err => err.getMessage)
      } else {
        Right(List[Entry[A]]()): Either[String, List[Entry[A]]]
      }
    } yield entries
  }

  private var entries : List[Entry[A]] = readFile().getOrElse(List())

  def add(item:A):Unit = {
    entries = (entries ++ List(Entry(item))).takeRight(limit)
    val pdir = path.parent.map(_.canonical)
    pdir match {
      case None => ()
      case Some(dir) => dir.isDir.map {
        case true =>
        case _ => dir.createDirectories
      }
    }
    path.json.write(entries)
  }

  def get(index:Int):Option[A] = entries.lift(index).map(_.value)

  def size:Int = entries.size
}

object History {
  def apply[A:JsonWriter:JsonReader](path:Path,limit:Int)(implicit trace: JavaNioTracer):History[A] =
    new HistoryImpl[A](path, limit)

  def dummy[A:JsonWriter:JsonReader]():History[A] = new History[A] {
    override def add(item: A): Unit = ()
    override def get(index: Int): Option[A] = None
    override def size: Int = 0
  }
}
