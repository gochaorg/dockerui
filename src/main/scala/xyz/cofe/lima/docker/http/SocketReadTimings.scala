package xyz.cofe.lima.docker.http

import Duration._
import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

/**
 * Тайминги чтения данных из сокета
 * @param sourceTimeout максимальная задерждка данных очередного байта из сокета, после которого будет отказ/завершение
 * @param readTimeout максимальное допустимое время
 * @param cpuThrottling задержка чтоб проц не перегревался
 * @param streamReadTimeout максимальное допустимое время, при потоковом чтении
 * @param streamSourceTimeout максимальная задерждка данных очередного байта из сокета, после которого будет отказ/завершение,  при потоковом чтении
 */
case class SocketReadTimings(
                              sourceTimeout: Option[Duration] = 30.seconds.some,
                              readTimeout: Option[Duration] = 60.seconds.some,
                              cpuThrottling: Option[Duration] = 1.milliseconds.some,
                              streamReadTimeout: Option[Duration] = None,
                              streamSourceTimeout: Option[Duration] = 30.seconds.some,
                            )
object SocketReadTimings {
  implicit val reader: JsonReader[SocketReadTimings] = jsonReader[SocketReadTimings]
  implicit val writer: JsonWriter[SocketReadTimings] = jsonWriter[SocketReadTimings]
}