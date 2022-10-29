package xyz.cofe.lima.thread

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}

case class ThreadID(id:Long, name:String)
object ThreadID {
  def current:ThreadID = {
    val th = Thread.currentThread()
    ThreadID(th.getId, th.getName)
  }

  implicit val reader: JsonReader[ThreadID] = jsonReader[ThreadID]
  implicit val writer: JsonWriter[ThreadID] = jsonWriter[ThreadID]
}
