package xyz.cofe.lima.docker.http

import Duration._

/**
 * Чтение данных из некого сокета,
 * читает данные ожидая их поступление, с приемлеменными задержками
 *
 * @param source источник данных
 * @param decoder декодер данных
 * @param sourceTimeout (мс) максимальная задерждка данных очередного байта из сокета, после которого будет отказ/завершение
 * @param readTimeout (мс) максимальное допустимое время исполнения метода read
 * @param cpuThrottling (мс) пропуск CPU
 * @tparam I тип исходных данных
 * @tparam O тип результирущих данных
 */
case class DelaiedReader[I,O](source:()=>Option[I],
                              decoder: Decoder[I,O,_],
                              sourceTimeout:Option[Duration]=None,
                              readTimeout:  Option[Duration]=None,
                              cpuThrottling:Option[Duration]=None
                            ) extends Function0[Option[O]] {
  var buffer: Seq[O] = Seq[O]()
  def read:Option[O] = {
    if( buffer.nonEmpty ){
      val el = buffer.head
      buffer = buffer.tail
      Some(el)
    }else{
      val readStart = TimePoint.now
      var stop = false
      var waitStart:Option[TimePoint] = None
      var result = None:Option[O]
      while(!stop) {
        source() match {
          case Some(value) =>
            waitStart = None
            decoder.accept(List(value))
            val decodedResult = decoder.fetch
            if( decodedResult.nonEmpty ){
              result = Some(decodedResult.head)
              buffer = result.tail.toList
              stop = true
            }else{
              if( (TimePoint.now-readStart)>readTimeout ){
                stop = true
              }
            }
          case None =>
            if( waitStart.isEmpty ) {
              waitStart = TimePoint.now.some
            } else if( (TimePoint.now - waitStart.getOrElse(TimePoint(0)))>sourceTimeout ){
              stop = true
            } else {
              if( (TimePoint.now-readStart)>readTimeout ){
                stop = true
              } else {
                cpuThrottling.sleep()
              }
            }
        }
      }
      result
    }
  }

  override def apply(): Option[O] = read
}
