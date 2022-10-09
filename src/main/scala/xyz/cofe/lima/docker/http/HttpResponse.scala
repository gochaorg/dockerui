package xyz.cofe.lima.docker.http

import java.io.ByteArrayOutputStream
import java.nio.charset.{Charset, StandardCharsets}
import java.util.regex.Pattern

case class HttpResponse( firstLine: String,
                         headers: List[(String,String)],
                         body: Seq[Byte],
                         bodyError:Option[String] = None,
                         bodyDecoded:Boolean = false
                       ) {
  lazy val dump:String = {
    val sb = new StringBuilder
    sb ++= "HttpResponse\n"
    sb ++= s"firstLine $firstLine\n"
    headers.foreach { case(k,v) => sb ++= s"header $k $v\n" }

    bodyError.foreach(err => sb ++= s"bodyError $err")
    sb ++= s"bodyDecoded ${bodyDecoded}"

    sb ++= s"body-size ${body.size}\n"
    sb ++= HexDump.toString(body, "body-data ") + "\n"
    sb.toString()
  }

  lazy val contentTypeRaw:Option[Seq[String]] = headers
    .find { case(name,_) => name.equalsIgnoreCase("Content-Type") }
    .map { _._2 }
    .map { line => line.split(";") }

  lazy val contentTypeMap:Option[Map[String,String]] =
    contentTypeRaw.map { lines =>
      lines.drop(1).map( line => (line.indexOf("="),line) )
        .filter { case(i,line) => i>0 && i<line.length-1 }
        .map { case(i,line) => (line.substring(0,i).trim, line.substring(i+1).trim) }
    }.map { _.toMap }

  lazy val contentTypeMapLowerCase: Option[Map[String, String]] =
    contentTypeMap.map { m => m.map { case(k,v) => k.toLowerCase -> v}.toMap }

  lazy val contentType:Option[String] =
    contentTypeRaw.flatMap { _.headOption }

  lazy val isText: Boolean = contentType.exists { ct =>
    ct.matches("(?is)text/.+|xml/.+|json/.+|application/json")
  }

  lazy val charset: Option[Charset] = isText match {
    case false => None
    case true =>
      Some( contentTypeMapLowerCase
        .flatMap { _.get("charset") }
        .map( cs => Charset.forName(cs) )
        .getOrElse( StandardCharsets.UTF_8 )
      )
  }

  lazy val firstLineDecoded: Option[(String, String, Option[String])] = {
    val ptrn = Pattern.compile(
      "(?is)(?<proto>HTTPS?/\\d(\\.\\d))\\s+(?<code>\\d+)(\\s+(?<msg>.+))?")
    val m = ptrn.matcher(firstLine)

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

  lazy val transferEncoding:Option[String] = headers
    .find { case(k,v) => k.equalsIgnoreCase("Transfer-Encoding") }
    .map{ case(_,v)=>v }

  lazy val isTransferEncodingChunked:Boolean =
    transferEncoding.exists(_.equalsIgnoreCase("chunked"))

  lazy val decodedChunks: Either[String, Array[Byte]] = {
    if(!isTransferEncodingChunked){
      Left("body is not chunked")
    } else {
      def fromHex(b: Byte): Option[Int] = {
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

      def isCR(b: Byte) = b == 13

      def isLF(b: Byte) = b == 10

      def readChunkSize(from: Int) = {
        var stop = false
        var digits = List[Int]()
        var next = 0
        var err = None: Option[String]

        while (!stop) {
          fromHex(body(from + digits.size)) match {
            case Some(digit) => digits = digit :: digits
            case None =>
              if (
                isCR(body(from + digits.size)) &&
                  isLF(body(from + digits.size + 1))
              ) {
                stop = true
                next = from + digits.size + 2
              } else {
                err = Some(s"expect CRLF in body at ${from + digits.size} position")
              }
          }
        }

        err match {
          case Some(errMessage) => Left(errMessage)
          case None => digits.size match {
            case 0 => Left(s"no hex digits in body at $from")
            case _ =>
              val chunckSize = digits.zipWithIndex.foldLeft(0) { case (sum, (digit, pos)) =>
                sum + digit * (pos match {
                  case 0 => 1
                  case _ => 16 << ((pos - 1) * 4)
                })
              }
              Right((chunckSize, next))
          }
        }
      }

      var ptr = 0
      var stop = false
      val buff = new ByteArrayOutputStream()
      val ba = new Array[Byte](1)
      var err = None: Option[String]

      while (!stop) {
        readChunkSize(ptr) match {
          case Left(errMsg) =>
            err = Some(errMsg)
            stop = true
          case Right((size, nextPtr)) =>
            if (size <= 0) {
              stop = true
            } else {
              (0 until size).foreach { idx =>
                ba(0) = body(idx + nextPtr)
                buff.write(ba)
              }
              if (isCR(body(nextPtr + size)) && isLF(body(nextPtr + size + 1))) {
                ptr = nextPtr + size + 2
              } else {
                err = Some(s"expect CRLF in body at ${nextPtr + size}")
                stop = true
              }
            }
        }
      }

      err match {
        case Some(errMsg) =>
          Left(errMsg)
        case None =>
          Right(buff.toByteArray)
      }
    }
  }

  private lazy val decodedBody:Option[Seq[Byte]] =
    if( bodyDecoded ){
      Some(body)
    } else {
      if( isTransferEncodingChunked ){
        decodedChunks match {
          case Left(err) => None
          case Right(value) => Some(value)
        }
      }else{
        Some(body)
      }
    }

  lazy val text: Option[String] = for {
    cs <- charset
    bytes <- decodedBody.map(_.toArray)
    str = new String(bytes, cs)
  } yield str
}

object HttpResponse {
  def fromDump(dump: String):Either[String,HttpResponse] = {
    val lines = dump.split("\r?\n")
    if( lines.size < 1 ) {
      Left("to small lines in dump")
    }else if( lines(0)!="HttpResponse" ){
      Left(s"expect first line \"HttpResponse\", actual \"${lines(0)}\"")
    }else {
      var firstLine = None:Option[String]
      var headers = List[(String,String)]()
      val body = new ByteArrayOutputStream()
      var bodyError = None:Option[String]
      var bodyDecoded = false

      lines.foreach { case(line) =>
        if( line.startsWith("firstLine ")){
          firstLine = Some(line.substring("firstLine ".length))
        }else if( line.startsWith("bodyError ")){
          bodyError = Some(line.substring("bodyError ".length))
        }else if( line.startsWith("bodyDecoded ")){
          bodyDecoded = line.substring("bodyDecoded ".length).trim.toBoolean
        }else if( line.startsWith("header ")){
          val headerLine = line.substring("header ".length)
          val kv = headerLine.split("\\s+",2)
          headers = (kv(0) -> kv(1)) :: headers
        }else if( line.startsWith("body-data ")){
          val dataLine = line.substring("body-data ".length)
          body.write(
            dataLine.split("\\s+").map { str => java.lang.Byte.parseByte(str, 16) }
          )
        }
      }
      if( firstLine.isEmpty ) {
        Left("not found first line of response")
      } else {
        Right(
          HttpResponse(
            firstLine.get,
            headers.reverse,
            body.toByteArray,
            bodyError = bodyError,
            bodyDecoded = bodyDecoded,
          )
        )
      }
    }
  }
}
