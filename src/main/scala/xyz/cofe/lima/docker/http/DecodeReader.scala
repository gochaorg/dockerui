package xyz.cofe.lima.docker.http

/**
 * Чтение данных из некого сокета,
 * читает данные ожидая их поступление, с приемлеменными задержками
 * @param source источник данных
 * @param decoder декодер данных
 * @param sourceTimeout (мс) максимальная задерждка данных очередного байта из сокета, после которого будет отказ/завершение
 * @param readTimeout (мс) максимальное допустимое время исполнения метода read
 * @param cpuThrottling (мс) пропуск CPU
 * @tparam I тип исходных данных
 * @tparam O тип результирущих данных
 */
case class DecodeReader[I,O](source:()=>Option[I],
                             decoder: Decoder[I,O,_],
                             sourceTimeout:Long=0,
                             readTimeout:Long=0,
                             cpuThrottling:Long=1
                            ) extends Function0[Option[O]] {
  var buffer: Seq[O] = Seq[O]()
  def read:Option[O] = {
    if( buffer.nonEmpty ){
      val el = buffer.head
      buffer = buffer.tail
      Some(el)
    }else{
      var readStart = System.currentTimeMillis()
      var stop = false
      var waitStart = 0L
      var result = None:Option[O]
      while(!stop) {
        source() match {
          case Some(value) =>
            waitStart = 0
            decoder.accept(List(value))
            val decodedResult = decoder.fetch
            if( decodedResult.nonEmpty ){
              result = Some(decodedResult.head)
              buffer = result.tail.toList
              stop = true
            }else{
              if( (System.currentTimeMillis()-readStart)>readTimeout && readTimeout>0 ){
                stop = true
              }
            }
          case None =>
            if( waitStart==0 ) {
              waitStart = System.currentTimeMillis()
            } else if( (System.currentTimeMillis() - waitStart)>sourceTimeout ){
              stop = true
            } else {
              if( (System.currentTimeMillis()-readStart)>readTimeout && readTimeout>0 ){
                stop = true
              } else {
                if (cpuThrottling > 0) {
                  Thread.sleep(cpuThrottling)
                }
              }
            }
        }
      }
      result
    }
  }

  override def apply(): Option[O] = read
}
