package xyz.cofe.lima.store

import tethys.{JsonObjectWriter, JsonWriter}

import scala.reflect.ClassTag

package object json {
  def classWriter[A](implicit ct: ClassTag[A]): JsonObjectWriter[A] = {
    JsonWriter.obj[A].addField("_type")(_ => ct.runtimeClass.getSimpleName)
  }
}
