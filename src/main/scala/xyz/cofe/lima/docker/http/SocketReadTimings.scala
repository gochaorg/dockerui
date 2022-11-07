package xyz.cofe.lima.docker.http

import Duration._
import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

/**
 * Тайминги чтения данных из сокета
 * @param cpuThrottling задержка чтоб проц не перегревался
 * @param readTimeout  максимальное допустимое время
 * @param sourceTimeout максимальная задерждка данных очередного байта из сокета, после которого будет отказ/завершение
 */
case class SocketReadTimings(
                              cpuThrottling: Option[Duration] = 1.milliseconds.some,
                              readTimeout: Option[Duration] = 30.seconds.some,
                              sourceTimeout: Option[Duration] = 30.seconds.some,
                            )
object SocketReadTimings {
  implicit val reader: JsonReader[SocketReadTimings] = jsonReader[SocketReadTimings]
  implicit val writer: JsonWriter[SocketReadTimings] = jsonWriter[SocketReadTimings]
}