package xyz.cofe.lima.docker.http

import java.io.ByteArrayOutputStream
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.{CharsetDecoder, StandardCharsets}

trait Decoder[I,O,T] {
  def fetch:Seq[O]
  def accept(input:Seq[I]):Unit
  def tail:T

  def compose[I2](dec:Decoder[I2,I,_]):Decoder[I2,O,T] = {
    val self = this
    new Decoder[I2, O,T] {
      override def fetch: Seq[O] = self.fetch
      override def accept(input: Seq[I2]): Unit = {
        dec.accept(input)
        self.accept(dec.fetch)
      }
      override def tail: T = self.tail
    }
  }
}

object Decoder {
  case class Byte2Char(charsetDecoder: CharsetDecoder) extends Decoder[Byte,Char,String] {
    val sb = new StringBuilder
    val bb: ByteBuffer = ByteBuffer.allocate(128)

    override def fetch: Seq[Char] = {
      val res = sb.toList
      sb.clear()
      res
    }
    override def accept(input: Seq[Byte]): Unit = {
      val available = bb.limit() - bb.position()
      if(available>0){
        if( input.size>available ) {
          input.grouped(available match {
            case a: Int if a < 10 => throw new IllegalStateException("to small")
            case _ => Math.max(available - 10, 1)
          }).foreach { block =>
            bb.put(block.toArray)
            bb.flip()
            val cb = charsetDecoder.decode(bb)
            bb.compact()
            sb.append(cb.toString)
          }
        }else{
          bb.put(input.toArray)
          bb.flip()
          val cb = charsetDecoder.decode(bb)
          bb.compact()
          sb.append(cb.toString)
        }
      }else{
        throw new IllegalStateException("no available free space")
      }
    }

    override def tail:String = sb.toString()
  }
  case class Char2Line() extends Decoder[Char,String,String] {
    var collected: List[String] = List[String]()
    val current = new StringBuilder()

    override def fetch: Seq[String] = {
      val res = collected
      collected = List[String]()
      res.reverse
    }

    override def accept(input: Seq[Char]): Unit = {
      input.foreach {
        case '\r' =>
        case '\n' =>
          collected = current.toString() :: collected
          current.clear()
        case c:Char =>
          current.append(c)
      }
    }

    override def tail:String = current.toString()
  }
  case class Buffer[A]() extends Decoder[A,A,Unit] {
    var buffer:List[A] = List[A]()
    override def fetch: Seq[A] = {
      val res = buffer
      buffer = List()
      res
    }
    override def accept(input: Seq[A]): Unit = {
      buffer = input.reverse.toList ::: buffer
    }
    override def tail: Unit = ()
  }
}


