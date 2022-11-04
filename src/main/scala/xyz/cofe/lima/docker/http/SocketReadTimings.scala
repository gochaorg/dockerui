package xyz.cofe.lima.docker.http

/**
 * Тайминги чтения данных из сокета
 * @param sourceTimeout
 */
case class SocketReadTimings(
                              sourceTimeout: Long = 1000L,
                              readTimeout: Long = 1000L * 30L,
                              cpuThrottling: Long = 1,
                              streamReadTimeout: Long = (-1),
                              streamSourceTimeout: Long = 1000L * 30L,
                            )
