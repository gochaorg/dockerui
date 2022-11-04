package xyz.cofe.lima.docker.http

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import tethys.{JsonReader, JsonWriter}
import xyz.cofe.lima.docker.http.Duration._
import xyz.cofe.lima.store.json._

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import java.util.regex.Pattern

case class HttpResponseStream(source        :()=>Option[Byte],
                              sourceTimeout :Option[Duration]=10.seconds.some,
                              readTimeout   :Option[Duration]=60.seconds.some,
                              cpuThrottling :Option[Duration]=1.milliseconds.some,
                              pid           :Long = -1
                             )(implicit httpLogger: HttpLogger)
{
  import HttpResponseStream._

  lazy val byte2charDecoder: Decoder.Byte2Char = Decoder.Byte2Char(StandardCharsets.UTF_8.newDecoder())
  lazy val lineDecoder: Decoder[Byte, String, String] = Decoder.Char2Line().compose( byte2charDecoder )
  lazy val lineReader: DelaiedReader[Byte, String] = DelaiedReader[Byte,String](
    source,
    lineDecoder,
    readTimeout=readTimeout, sourceTimeout=sourceTimeout, cpuThrottling=cpuThrottling
  )
  lazy val byteReader: DelaiedReader[Byte, Byte] = DelaiedReader[Byte,Byte](
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
            case Event.Error(pis,string) =>
              consumer(Event.Error(pid,string))
              hasError = true
              Behavior.Stop
            case Event.Header(pid, name, value) =>
              headers = (name -> value) :: headers
              consumer(Event.Header(pid, name,value))
              Behavior.Continue
            case Event.HeaderEnd(pid) =>
              Behavior.Stop
            case _ =>
              hasError = true
              consumer(Event.Error(pid,s"undefined Behavior"))
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
          consumer(httpLogger.event(Event.FirstLine(pid,line)))
        } else {
          consumer(httpLogger.event(Event.Error(pid,s"first line ($line) not match $firstLineRegex")))
          Behavior.Stop
        }
      case None =>
        consumer(Event.Error(pid,HttpResponse.NO_RESPONSE))
        Behavior.Stop
    }

  def readHeader(consumer: Consumer):Behavior = {
    lineReader.read match {
      case Some(line) =>
        if(line.isEmpty){
          consumer(httpLogger.event(Event.HeaderEnd(pid)))
        }else{
          val idx = line.indexOf(":")
          if( idx<1 || idx>=line.length ){
            consumer(httpLogger.event(Event.Error(pid,s"expect header, line with: name \":\" value, but accept $line")))
            Behavior.Stop
          }else{
            val name = line.substring(0,idx).trim
            val value = line.substring(idx+1).trim
            consumer(httpLogger.event(Event.Header(pid,name,value)))
          }
        }
      case None =>
        consumer(httpLogger.event(Event.Error(pid,s"No response: header not readed")))
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
          consumer(httpLogger.event(Event.Data(pid,ba))) match {
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
      case Some(value) => consumer(httpLogger.event(Event.Error(pid,value)))
      case None => consumer(httpLogger.event(Event.DataEnd(pid)))
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
          consumer(httpLogger.event(Event.Data(pid,ba))) match {
            case Behavior.Continue => ()
            case Behavior.Stop =>
              stop = true
          }
        case None =>
          stop = true
      }
    }
    consumer(httpLogger.event(Event.DataEnd(pid)))
  }

  //#region read transfer encoding

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
          case None => if (isCR(b0)) {
            Right(Nil)
          } else {
            Left("expect CR (\\r)")
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
          consumer(httpLogger.event(Event.Data(pid,chunkData))) match {
            case Behavior.Stop => Right(())
            case Behavior.Continue =>
              reader()
          }
      }
    } yield ()

    reader() match {
      case Left(errMessage) =>
        consumer(httpLogger.event(Event.Error(pid,errMessage)))
      case Right(_) =>
        consumer(httpLogger.event(Event.DataEnd(pid)))
    }
  }

  //#endregion
}

object HttpResponseStream {
  sealed trait Behavior
  object Behavior {
    case object Continue extends Behavior
    case object Stop extends Behavior
  }

  sealed trait Event
  object Event {
    //#region Error

    case class Error(pid:Long, string: String) extends Event
    object Error {
      implicit val reader: JsonReader[Error] = jsonReader[Error]
      implicit val writer: JsonWriter[Error] = classWriter[Error] ++ jsonWriter[Error]
    }

    //#endregion
    //#region FirstLine

    case class FirstLine(pid:Long, string:String) extends Event {
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
    object FirstLine {
      implicit val reader: JsonReader[FirstLine] = jsonReader[FirstLine]
      implicit val writer: JsonWriter[FirstLine] = classWriter[FirstLine] ++ jsonWriter[FirstLine]
    }

    //#endregion
    //#region Header

    case class Header(pid:Long, name:String,value:String) extends Event
    object Header {
      implicit val reader: JsonReader[Header] = jsonReader[Header]
      implicit val writer: JsonWriter[Header] = classWriter[Header] ++ jsonWriter[Header]
    }

    //#endregion
    //#region HeaderEnd

    case class HeaderEnd(pid:Long) extends Event
    object HeaderEnd {
      implicit val reader: JsonReader[HeaderEnd] = jsonReader[HeaderEnd]
      implicit val writer: JsonWriter[HeaderEnd] = classWriter[HeaderEnd] ++  jsonWriter[HeaderEnd]
    }

    //#endregion
    //#region Data

    case class Data(pid:Long, bytes:Array[Byte]) extends Event
    object Data {
      case class DataLogView(pid:Long, bytes:Option[String],_type:String="Data") {
        def toData:Data = new Data(pid, bytes.map(bs => HexDump.bytesFrom(bs)).orNull)
      }
      object DataLogView {
        def apply(data:Data):DataLogView =
          new DataLogView(data.pid, if(data.bytes!=null) Some(HexDump.toString(data.bytes)) else None )

        implicit val reader: JsonReader[DataLogView] = jsonReader[DataLogView]
        implicit val writer: JsonWriter[DataLogView] = jsonWriter[DataLogView]
      }

      implicit val reader: JsonReader[Data] = DataLogView.reader.map[Data]( r=>r.toData )
      implicit val writer: JsonWriter[Data] = DataLogView.writer.contramap[Data]( r => DataLogView(r) )
    }

    //#endregion
    //#region DataEnd

    case class DataEnd(pid:Long) extends Event
    object DataEnd {
      implicit val reader: JsonReader[DataEnd] = jsonReader[DataEnd]
      implicit val writer: JsonWriter[DataEnd] = classWriter[DataEnd] ++ jsonWriter[DataEnd]
    }

    //#endregion

    implicit val writer:JsonWriter[Event] = (value: Event, tokenWriter: TokenWriter) => {
      value match {
        case ev: Error => Error.writer.write(ev, tokenWriter)
        case ev: FirstLine => FirstLine.writer.write(ev, tokenWriter)
        case ev: Header => Header.writer.write(ev, tokenWriter)
        case ev: HeaderEnd => HeaderEnd.writer.write(ev, tokenWriter)
        case ev: Data => Data.writer.write(ev, tokenWriter)
        case ev: DataEnd => DataEnd.writer.write(ev, tokenWriter)
      }
    }
    implicit val reader:JsonReader[Event] = JsonReader.builder.addField[String]("_type").selectReader[Event] {
      case "Error" => Error.reader
      case "FirstLine" => FirstLine.reader
      case "Header" => Header.reader
      case "HeaderEnd" => HeaderEnd.reader
      case "Data" => Data.reader
      case "DataEnd" => DataEnd.reader
    }
  }
}