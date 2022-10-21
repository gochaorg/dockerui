package xyz.cofe.lima.docker.http

import java.io.InputStream

class InputStreamSupplier(val stream:InputStream) extends Function0[Option[Byte]] {
  private val buffer:Array[Byte] = new Array[Byte](1024*4)
  private var bufferDataSize:Int = 0
  private var bufferPos:Int = 1024*4
  private var closed:Boolean = false
  override def apply(): Option[Byte] = {
    this.synchronized {
      if( closed ){
        None
      }else{
        val available = bufferDataSize - bufferPos
        if( available>0 ){
          val pos = bufferPos
          bufferPos += 1
          Some(buffer(pos))
        }else{
          val reads = stream.read(buffer)
          if( reads<=0 ){
            closed = true
            None
          }else{
            bufferDataSize = reads
            bufferPos = 1
            Some(buffer(0))
          }
        }
      }
    }
  }
}
