package xyz.cofe.lima.docker.http

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

case class HttpResponseReader( source:()=>Option[Byte],
                               sourceTimeout:Long=1000L*10L,
                               readTimeout:  Long=1000L*60L,
                               cpuThrottling:Long=1,
                               pid:Long = -1,
                             )
{
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

  def read:Either[String,HttpResponse] = {
    readFirstLine.flatMap( firstLine => {
      var headers = List[(String,String)]()
      var stop = false
      var err:Option[String] = None
      while( !stop && err.isEmpty ){
        readHeader match {
          case Left(errorMessage) => err = Some(errorMessage)
          case Right(None) => stop = true
          case Right(Some((k,v))) =>
            headers = (k,v) :: headers
        }
      }
      err match {
        case Some(errMsg) => Left(errMsg)
        case None => Right( (firstLine,headers.reverse) )
      }
    }).flatMap { headerBlock =>
      val result = contentLength(headerBlock._2).map { contentLength =>
        // val body = readWithDefinedContentLength(contentLength)
        ( headerBlock._1, headerBlock._2, readWithDefinedContentLength(contentLength))
      }.getOrElse {
        // contentLength not defined
        ( headerBlock._1, headerBlock._2, readWithUndefinedContentLength(headerBlock._2))
      }

      val (firstLine, headers, bodyEt) = result
      (for {
        body <- bodyEt
      } yield HttpResponse(firstLine, headers, body, bodyDecoded = true, pid = pid))
        .left.map { case(str,_) => str }
    }
  }

  private val firstLineRegex = "(?is)HTTPS?/\\d(\\.\\d)?\\s+\\d+(\\s+.*)?"
  def readFirstLine:Either[String,String] =
    (lineReader.read match {
      case Some(line) => Right(line)
      case None => Left(HttpResponse.NO_RESPONSE)
    }).flatMap( line => line.matches(firstLineRegex) match {
      case true => Right(line)
      case false => Left(s"first line ($line) not match $firstLineRegex")
    })
  def readHeader:Either[String,Option[(String,String)]] = {
    (lineReader.read match {
      case Some(line) =>
        if( line.isEmpty ){
          Right(None)
        } else {
          val idx = line.indexOf(":")
          if( idx<1 || idx>=line.length ){
            Left(s"expect header, line with: name \":\" value, but accept $line") //todo err
          }else{
            val name = line.substring(0,idx).trim
            val value = line.substring(idx+1).trim
            Right(Some((name,value)))
          }
        }
      case None => Left(s"No response: header not readed") //todo err
    })
  }

  private def contentLength(headers:List[(String,String)]):Option[Long] =
    headers
      .find { case(name,_) => name.equalsIgnoreCase("Content-Length") }
      .map { _._2 }
      .filter { str => str.matches("\\d+") }
      .map { str => str.toLong }

  private def readWithDefinedContentLength(contentLength:Long): Either[(String,Array[Byte]),Array[Byte]] = {
    val bytes = new ByteArrayOutputStream()
    var reads = 0L
    var stop = false
    val ba = new Array[Byte](1)
    while( !stop && reads<contentLength ){
      byteReader.read match {
        case Some(b) =>
          ba(0) = b
          bytes.write(ba)
          reads += 1
        case None =>
          stop = true
      }
    }
    Right(bytes.toByteArray)
  }

  private def transferEncoding(headers:List[(String,String)]):Option[String] =
    headers
      .find { case(name,_) => name.equalsIgnoreCase("Transfer-Encoding") }
      .map { _._2 }

  private def isTransferEncodingChunked(headers:List[(String,String)]):Boolean =
    transferEncoding(headers).exists(_.equalsIgnoreCase("chunked"))

  private def readWithUndefinedContentLength(headers: List[(String,String)]): Either[(String,Array[Byte]),Array[Byte]] = {
    if( isTransferEncodingChunked(headers) ){
      readChunked()
    }else{
      readWhileHasData()
    }
  }

  private def readWhileHasData():Either[(String,Array[Byte]),Array[Byte]] = {
    val bytes = new ByteArrayOutputStream()
    var stop = false
    val ba = new Array[Byte](1)
    while( !stop ){
      byteReader.read match {
        case Some(b) =>
          ba(0) = b
          bytes.write(ba)
        case None =>
          stop = true
      }
    }
    Right(bytes.toByteArray)
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
          case None => Left("no data") //todo err
        }
        digits <- fromHex(b0) match {
          case Some(d) =>
            for {
              dig <- readChunkSize
              res <- dig match {
                case Nil =>
                  bytes() match {
                    case None => Left("no data, expect LF (\\n)") //todo err
                    case Some(b1) =>
                      if( isLF(b1) ){
                        Right( d :: dig )
                      }else{
                        Left("expect LF (\\n)") //todo err
                      }
                  }
                case _ => Right( d :: dig )
              }
            } yield res
          case None => isCR(b0) match {
            case true => Right( Nil )
            case false => Left("expect CR (\\r)") //todo err
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
            Left(s"reads ${readsBytes.length} less then expected ${expectSize}") //todo err
          else
            Right(readsBytes)
        case (b0,b1) =>
          Left(s"expect CRLF, but found ${b0}, ${b1}") //todo err
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
  private def readChunked():Either[(String,Array[Byte]),Array[Byte]] = {
    val bytes = new ByteArrayOutputStream()
    val chunkSizeReader = readChunkSize(byteReader)
    val chunkDataReader = readChunkData(byteReader)

    var stop = false
    var err:Option[String] = None
    while(!stop) {
      chunkSizeReader() match {
        case Left(value) =>
          err = Some(value)
          stop = true
        case Right(value) =>
          if( value>0 ) {
            chunkDataReader(value) match {
              case Left(value) =>
                err = Some(value)
                stop = true
              case Right(value) =>
                bytes.write(value)
            }
          }else{
            stop = true
          }
      }
    }

    err match {
      case Some(value) => Left((value,bytes.toByteArray))
      case None => Right(bytes.toByteArray)
    }

//    def reader():Either[String,Unit] = for {
//      chunkSize <- chunkSizeReader()
//      chunkData <- chunkSize match {
//        case 0 => Right(new Array[Byte](0))
//        case _ => chunkDataReader(chunkSize)
//      }
//      _ = chunkSize match {
//        case 0 => Right(())
//        case _ =>
//          bytes.write(chunkData)
//          reader()
//      }
//    } yield ()
//
//    reader().map { _ =>
//      bytes.toByteArray
//    }.left.map( err => (err, bytes.toByteArray) )
  }

}
