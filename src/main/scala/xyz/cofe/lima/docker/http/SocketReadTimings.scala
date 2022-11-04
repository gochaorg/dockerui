package xyz.cofe.lima.docker.http

import Duration._

/**
 * Тайминги чтения данных из сокета
 * @param sourceTimeout (мс) максимальная задерждка данных очередного байта из сокета, после которого будет отказ/завершение
 * @param readTimeout (мс) максимальное допустимое время
 */
case class SocketReadTimings(
                              sourceTimeout: Option[Duration] = 1.seconds.some,
                              readTimeout: Option[Duration] = 30.seconds.some,
                              cpuThrottling: Option[Duration] = 1.milliseconds.some,
                              streamReadTimeout: Option[Duration] = None,
                              streamSourceTimeout: Option[Duration] = 30.seconds.some,
                            )
