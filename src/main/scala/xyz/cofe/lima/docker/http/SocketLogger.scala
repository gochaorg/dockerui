package xyz.cofe.lima.docker.http

import java.nio.ByteBuffer
import java.util.concurrent.atomic.AtomicInteger

trait SocketLogger {
  def send(bytes:ByteBuffer):Unit
  def receive(bytes:ByteBuffer):Unit
}

object SocketLogger {
  implicit val defaultLogger = new SocketLogger {
    override def send(bytes: ByteBuffer): Unit = ()
    override def receive(bytes: ByteBuffer): Unit = ()
  }
  def stdout:SocketLogger = new SocketLogger {
    val idx = new AtomicInteger()
    val digits = "0123456789abcdef"
    def dump(bytes:ByteBuffer, prefix:String=""):String = {
      if( bytes.remaining()>0 ) {
        val buff = new Array[Byte](bytes.remaining())
        bytes.get(bytes.position(),buff,0,buff.length)
        buff.grouped(32).map { dataLine =>
          prefix + dataLine.map { byte =>
            val lo = byte & 0x0f
            val hi = (byte & 0xf0) >> 4
            digits.charAt(hi).toString + digits.charAt(lo).toString
          }.mkString(" ")
        }.mkString("\n")
      }else{
        ""
      }
    }
    override def send(bytes: ByteBuffer): Unit = {
      println(
        s"send(${idx.incrementAndGet()}) ${bytes.remaining()} bytes\n${dump(bytes,"  ")}"
      )
    }
    override def receive(bytes: ByteBuffer): Unit = {
      println(
        s"receive(${idx.incrementAndGet()}) ${bytes.remaining()} bytes\n${dump(bytes,"  ")}"
      )
    }
  }
}