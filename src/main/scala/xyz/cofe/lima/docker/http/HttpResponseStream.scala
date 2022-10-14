package xyz.cofe.lima.docker.http

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

case class HttpResponseStream(source:()=>Option[Byte],
                              sourceTimeout:Long=1000L*10L,
                              readTimeout:  Long=1000L*60L,
                              cpuThrottling:Long=1
                             )
{
  import HttpResponseStream._

  lazy val byte2charDecoder: Decoder.Byte2Char = Decoder.Byte2Char(StandardCharsets.UTF_8.newDecoder())
  lazy val lineDecoder: Decoder[Byte, String, String] = Decoder.Char2Line().compose( byte2charDecoder )
  lazy val lineReader: DecodeReader[Byte, String] = DecodeReader[Byte,String](
    source,
    lineDecoder,
    readTimeout=readTimeout, sourceTimeout=sourceTimeout, cpuThrottling=cpuThrottling
  )
  lazy val byteReader: DecodeReader[Byte, Byte] = DecodeReader[Byte,Byte](
    source,
    Decoder.Buffer[Byte](),
    readTimeout=readTimeout, sourceTimeout=sourceTimeout, cpuThrottling=cpuThrottling
  )

  type Consumer = HttpResponseStream.Event=>HttpResponseStream.Behavior

  def read(consumer:Consumer):Unit = {
    readFirstLine(consumer) match {
      case Behavior.Stop => ()
      case Behavior.Continue =>
        var headers = List[(String,String)]()
        var stop = false
        var hasError = false
        while( !stop ){
          (readHeader {
            case Event.Error(string) =>
              consumer(Event.Error(string))
              hasError = true
              Behavior.Stop
            case Event.Header(name, value) =>
              headers = (name -> value) :: headers
              consumer(Event.Header(name,value))
              Behavior.Continue
            case Event.HeaderEnd =>
              Behavior.Stop
            case _ =>
              hasError = true
              consumer(Event.Error(s"undefined Behavior"))
              Behavior.Stop
          }) match {
            case Behavior.Continue =>
            case Behavior.Stop =>
              stop = true
          }
        }
        if( !hasError ){
          headers = headers.reverse
          contentLength(headers) match {
            case Some(length) => readWithDefinedContentLength(consumer,length)
            case None => readWithUndefinedContentLength(consumer,headers)
          }
        }
    }
  }

  private val firstLineRegex = "(?is)HTTPS?/\\d(\\.\\d)?\\s+\\d+(\\s+.*)?"
  def readFirstLine(consumer: Consumer):Behavior =
    lineReader.read match {
      case Some(line) =>
        if (line.matches(firstLineRegex)) {
          consumer(Event.FirstLine(line))
        } else {
          consumer(Event.Error(s"first line ($line) not match $firstLineRegex"))
          Behavior.Stop
        }
      case None =>
        consumer(Event.Error(HttpResponse.NO_RESPONSE))
        Behavior.Stop
    }

  def readHeader(consumer: Consumer):Behavior = {
    lineReader.read match {
      case Some(line) =>
        if(line.isEmpty){
          consumer(Event.HeaderEnd)
        }else{
          val idx = line.indexOf(":")
          if( idx<1 || idx>=line.length ){
            consumer(Event.Error(s"expect header, line with: name \":\" value, but accept $line"))
            Behavior.Stop
          }else{
            val name = line.substring(0,idx).trim
            val value = line.substring(idx+1).trim
            consumer(Event.Header(name,value))
          }
        }
      case None =>
        consumer(Event.Error(s"No response: header not readed"))
        Behavior.Stop
    }
  }

  private def contentLength(headers:List[(String,String)]):Option[Long] =
    headers
      .find { case(name,_) => name.equalsIgnoreCase("Content-Length") }
      .map { _._2 }
      .filter { str => str.matches("\\d+") }
      .map { str => str.toLong }
  private def readWithDefinedContentLength(consumer: Consumer, contentLength:Long):Unit = {
    var reads = 0L
    var stop = false
    var err:Option[String] = None
    val ba = new Array[Byte](1)
    while( !stop && reads<contentLength ){
      byteReader.read match {
        case Some(b) =>
          ba(0) = b
          reads += 1
          consumer(Event.Data(ba)) match {
            case Behavior.Stop =>
              stop = true
            case _ => ()
          }
        case None =>
          stop = true
          err = Some(s"no data, reads $reads bytes, expect $contentLength")
      }
    }

    err match {
      case Some(value) => consumer(Event.Error(value))
      case None => consumer(Event.DataEnd)
    }
  }

  private def transferEncoding(headers:List[(String,String)]):Option[String] =
    headers
      .find { case(name,_) => name.equalsIgnoreCase("Transfer-Encoding") }
      .map { _._2 }
  private def isTransferEncodingChunked(headers:List[(String,String)]):Boolean =
    transferEncoding(headers).exists(_.equalsIgnoreCase("chunked"))

  private def readWithUndefinedContentLength(consumer: Consumer,headers: List[(String,String)]):Unit = {
    if( isTransferEncodingChunked(headers) ){
      readChunked(consumer)
    }else{
      readWhileHasData(consumer)
    }
  }

  private def readWhileHasData(consumer: Consumer):Unit = {
    var stop = false
    val ba = new Array[Byte](1)
    while( !stop ){
      byteReader.read match {
        case Some(b) =>
          ba(0) = b
          consumer(Event.Data(ba)) match {
            case Behavior.Continue => ()
            case Behavior.Stop =>
              stop = true
          }
        case None =>
          stop = true
      }
    }
    consumer(Event.DataEnd)
  }

  private def fromHex(b: Byte): Option[Int] = {
    b match {
      case 48 => Some(0)
      case 49 => Some(1)
      case 50 => Some(2)
      case 51 => Some(3)
      case 52 => Some(4)
      case 53 => Some(5)
      case 54 => Some(6)
      case 55 => Some(7)
      case 56 => Some(8)
      case 57 => Some(9)

      case 65 => Some(10)
      case 66 => Some(11)
      case 67 => Some(12)
      case 68 => Some(13)
      case 69 => Some(14)
      case 70 => Some(15)

      case 97 => Some(10)
      case 98 => Some(11)
      case 99 => Some(12)
      case 100 => Some(13)
      case 101 => Some(14)
      case 102 => Some(15)
      case _ => None
    }
  }
  private def isCR(b: Byte) = b == 13
  private def isLF(b: Byte) = b == 10
  private def readChunkSize(bytes:()=>Option[Byte]):()=>Either[String,Int] = {
    def readChunkSize:Either[String,List[Int]] =
      for {
        b0 <- bytes() match {
          case Some(value) => Right(value)
          case None => Left("no data")
        }
        digits <- fromHex(b0) match {
          case Some(d) =>
            for {
              dig <- readChunkSize
              res <- dig match {
                case Nil =>
                  bytes() match {
                    case None => Left("no data, expect LF (\\n)")
                    case Some(b1) =>
                      if( isLF(b1) ){
                        Right( d :: dig )
                      }else{
                        Left("expect LF (\\n)")
                      }
                  }
                case _ => Right( d :: dig )
              }
            } yield res
          case None => isCR(b0) match {
            case true => Right( Nil )
            case false => Left("expect CR (\\r)")
          }
        }
      } yield digits

    () => readChunkSize.map { digits =>
      digits.reverse.zipWithIndex.foldLeft(0){ case(sum,(digit, pos)) =>
        pos match {
          case 0 => digit
          case _ => sum + (digit * (16 << ((pos-1) * 4)))
        }
      }
    }
  }
  private def readChunkData(bytes:()=>Option[Byte]):(Int)=>Either[String,Array[Byte]] = {
    def reader(expectSize:Int):Either[String,Array[Byte]] = {
      val data = new ByteArrayOutputStream()
      var reads = 0
      var stop = false
      val ba = new Array[Byte](1)
      while(!stop && reads<expectSize) {
        bytes.apply() match {
          case Some(value) =>
            ba(0) = value
            data.write(ba)
            reads += 1
          case None =>
            stop = true
        }
      }
      val readsBytes = data.toByteArray

      (bytes(), bytes()) match {
        case (Some(13), Some(10)) =>
          if( readsBytes.length < expectSize )
            Left(s"reads ${readsBytes.length} less then expected ${expectSize}")
          else
            Right(readsBytes)
        case (b0,b1) =>
          Left(s"expect CRLF, but found ${b0}, ${b1}")
      }
    }
    reader
  }

  /**
   * Чтение Transfer-Encoding: chunked
   *
   * Входные данные синтаксис
   *
   * <pre>
   * chunks          ::= { chunk }
   * chunk           ::= chunk_with_data | chunk_last
   * chunk_with_data ::= chunk_size chunk_data
   * chunk_last      ::= 0x30 0x0d 0x0a 0x0d 0x0a
   * chunk_size      ::= hex_digit { hex_digit } 0x0d 0x0a
   * hex_digit       ::= 0x30 .. 0x39 | 0x41 .. 0x46 | 0x61 .. 0x66
   * chunk_data      ::= { byte } #(repeat chunk_size) 0x0d 0x0a
   * </pre>
   */
  private def readChunked(consumer: Consumer):Unit = {
    val chunkSizeReader = readChunkSize(byteReader)
    val chunkDataReader = readChunkData(byteReader)
    def reader():Either[String,Unit] = for {
      chunkSize <- chunkSizeReader()
      chunkData <- chunkSize match {
        case 0 => Right(new Array[Byte](0))
        case _ => chunkDataReader(chunkSize)
      }
      _ = chunkSize match {
        case 0 => Right(())
        case _ =>
          consumer(Event.Data(chunkData)) match {
            case Behavior.Stop => Right(())
            case Behavior.Continue =>
              reader()
          }
      }
    } yield ()

    reader() match {
      case Left(errMessage) =>
        consumer(Event.Error(errMessage))
      case Right(_) =>
        consumer(Event.DataEnd)
    }
  }

}

object HttpResponseStream {
  sealed trait Behavior
  object Behavior {
    case object Continue extends Behavior
    case object Stop extends Behavior
  }

  sealed trait Event
  object Event {
    case class Error(string: String) extends Event
    case class FirstLine(string:String) extends Event {
      lazy val firstLineDecoded: Option[(String, String, Option[String])] = {
        val ptrn = Pattern.compile(
          "(?is)(?<proto>HTTPS?/\\d(\\.\\d))\\s+(?<code>\\d+)(\\s+(?<msg>.+))?")
        val m = ptrn.matcher(string)

        if(m.matches()) {
          val proto = m.group("proto")
          val code = m.group("code")
          val msg = m.group("msg")
          Some((proto, code, Option(msg)))
        }else{
          None
        }
      }

      lazy val proto: Option[String] = firstLineDecoded.map( _._1 )
      lazy val code: Option[Int] = firstLineDecoded.map( _._2.toInt )
      lazy val message: Option[String] = firstLineDecoded.flatMap( _._3 )
      lazy val isOk: Boolean = code.contains(200)
    }
    case class Header(name:String,value:String) extends Event
    case object HeaderEnd extends Event
    case class Data(bytes:Array[Byte]) extends Event
    case object DataEnd extends Event
  }
}