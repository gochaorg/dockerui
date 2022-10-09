package xyz.cofe.lima.docker.http

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets

case class HttpResponseReader( source:()=>Option[Byte],
                               sourceTimeout:Long=1000L*10L,
                               readTimeout:  Long=1000L*60L,
                               cpuThrottling:Long=1
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
      val result = contentLength(headerBlock._2).map { len =>
        val bytes = new ByteArrayOutputStream()
        var reads = 0L
        var stop = false
        val ba = new Array[Byte](1)
        while( !stop && reads<len ){
          byteReader.read match {
            case Some(b) =>
              ba(0) = b
              bytes.write(ba)
              reads += 1
            case None =>
              stop = true
          }
        }
        ( headerBlock._1, headerBlock._2, bytes.toByteArray)
      }.getOrElse {
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
        ( headerBlock._1, headerBlock._2, bytes.toByteArray)
      }
      Right(HttpResponse(result._1, result._2, result._3))
    }
  }

  private val firstLineRegex = "(?is)HTTPS?/\\d(\\.\\d)?\\s+\\d+(\\s+.*)?"
  def readFirstLine:Either[String,String] =
    (lineReader.read match {
      case Some(line) => Right(line)
      case None => Left("No response: first line not read")
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
            Left(s"expect header, line with: name \":\" value, but accept $line")
          }else{
            val name = line.substring(0,idx).trim
            val value = line.substring(idx+1).trim
            Right(Some((name,value)))
          }
        }
      case None => Left(s"No response: header not readed")
    })
  }

  private def contentLength(headers:List[(String,String)]):Option[Long] =
    headers
      .find { case(name,_) => name.equalsIgnoreCase("Content-Length") }
      .map { _._2 }
      .filter { str => str.matches("\\d+") }
      .map { str => str.toLong }
}
