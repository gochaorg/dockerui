package xyz.cofe.lima.docker.http

import java.nio.ByteBuffer
import java.nio.channels.SocketChannel

case class SocketChannelSupplier( socket:SocketChannel, bufferSize:Int=1024*4 )
                                ( implicit socketLogger: SocketLogger )
  extends Function0[Option[Byte]]
{
  lazy val byteBuffer = ByteBuffer.allocate(bufferSize)
  var innerBuffer = List[Byte]()

  def apply():Option[Byte] = {
    innerBuffer.nonEmpty match {
      case true =>
        val res = innerBuffer.head
        innerBuffer = innerBuffer.tail
        Some(res)
      case false =>
        val read = socket.read(byteBuffer)
        if( read<=0 ){
          None
        }else{
          byteBuffer.flip()
          socketLogger.receive(byteBuffer)
          while( byteBuffer.remaining()>0 ){
            innerBuffer = byteBuffer.get() :: innerBuffer
          }
          innerBuffer = innerBuffer.reverse
          byteBuffer.compact()
          innerBuffer.nonEmpty match {
            case true =>
              val res = innerBuffer.head
              innerBuffer = innerBuffer.tail
              Some(res)
            case false =>
              None
          }
        }
    }
  }
}
