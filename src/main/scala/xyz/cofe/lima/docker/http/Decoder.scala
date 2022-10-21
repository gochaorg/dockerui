package xyz.cofe.lima.docker.http

import java.nio.ByteBuffer
import java.nio.charset.CharsetDecoder

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
  case class Position(pos:Long) extends AnyVal with Ordered[Position]{
    def + (off:Int):Position = Position(pos+off)
    def - (off:Int):Position = Position(pos-off)
    def + (off:Long):Position = Position(pos+off)
    def - (off:Long):Position = Position(pos-off)
    override def compare(that: Position): Int = pos.compareTo(that.pos)
  }

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

  object Char2JsonEntry {
    def apply():Char2JsonEntry = new Char2JsonEntry()
  }
  class Char2JsonEntry() extends Decoder[Char,String,String] {
    sealed trait State
    case object Init extends State
    case object Undef extends State
    case class JsStr(quote:Char) extends State
    case class JsStrEsc(str:JsStr) extends State
    case class JsSkipEsc(str:JsStr,cnt:Int) extends State
    case class JsUnicodeEsc(str:JsStr) extends State

    var collected: List[String] = List[String]()
    val current = new StringBuilder()
    var state:State = Init
    var bracketLevel = 0

    var currentPos:Position = Position(0)
    var currentStartPos:Position = Position(0)

    def onGot(value:String,from:Position,to:Position):Unit = {

    }

    override def fetch: Seq[String] = {
      val res = collected
      collected = List[String]()
      res.reverse
    }

    override def accept(input: Seq[Char]): Unit = {
      input.foreach { chr =>
        state match {
          case Init => chr match {
            case _ if chr.isWhitespace =>
              current.append(chr)
              currentPos += 1
            case '{' =>
              current.append(chr)
              currentPos += 1
              bracketLevel += 1
            case '}' =>
              current.append(chr)
              currentPos += 1
              bracketLevel -= 1
              if( bracketLevel==0 ){
                collected = current.toString() :: collected
                onGot(current.toString(),currentStartPos,currentPos+1)
                current.clear()
                currentStartPos = currentPos
              }
            case '"' =>
              current.append(chr)
              currentPos += 1
              state = JsStr('"')
            case '\'' =>
              current.append(chr)
              currentPos += 1
              state = JsStr('\'')
            case _ =>
              current.append(chr)
              currentPos += 1
          }
          case s@JsStr(quote) => chr match {
            case _ if chr==quote =>
              current.append(chr)
              currentPos += 1
              state = Init
            case '\\' =>
              current.append(chr)
              currentPos += 1
              state = JsStrEsc(str = s)
            case _ =>
              current.append(chr)
              currentPos += 1
          }
          case esc:JsStrEsc => chr match {
            case '0' =>
              current.append(chr)
              currentPos += 1
              state = esc.str
            case '"' =>
              current.append(chr)
              currentPos += 1
              state = esc.str
            case '\'' =>
              current.append(chr)
              currentPos += 1
              state = esc.str
            case 'x' =>
              current.append(chr)
              currentPos += 1
              state = JsSkipEsc(str = esc.str,2)
            case 'u' =>
              current.append(chr)
              currentPos += 1
              state = JsUnicodeEsc(str = esc.str)
            case _ if chr.isDigit =>
              current.append(chr)
              currentPos += 1
              state = JsSkipEsc(str = esc.str,2)
            case _ =>
              current.append(chr)
              currentPos += 1
              state = esc.str
          }
          case esc@JsSkipEsc(str,cnt) if cnt>0 =>
            current.append(chr)
            currentPos += 1
            state = esc.copy(cnt=cnt-1)
          case JsSkipEsc(str,0) =>
            current.append(chr)
            currentPos += 1
            state = str
          case esc@JsUnicodeEsc(str) => chr match {
            case '{' =>
              current.append(chr)
              currentPos += 1
              state = JsSkipEsc(str,6)
            case _ =>
              current.append(chr)
              currentPos += 1
              state = JsSkipEsc(str,4)
          }
        }
      }
    }

    override def tail:String = current.toString()
  }
}


