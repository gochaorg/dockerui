package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.{DecodeReader, Decoder, HttpResponseReader, SingleSupplier}

import java.nio.ByteBuffer
import java.nio.charset.{CharsetDecoder, StandardCharsets}

class ResponseParseTest extends AnyFunSuite {
  lazy val bytesHex =
                 """48 54 54 50 2f 31 2e 31 20 32 30 30 20 4f 4b 0d 0a 41 70 69 2d 56 65 72 73 69 6f 6e 3a 20 31 2e
                   |34 31 0d 0a 43 6f 6e 74 65 6e 74 2d 54 79 70 65 3a 20 61 70 70 6c 69 63 61 74 69 6f 6e 2f 6a 73
                   |6f 6e 0d 0a 44 6f 63 6b 65 72 2d 45 78 70 65 72 69 6d 65 6e 74 61 6c 3a 20 66 61 6c 73 65 0d 0a
                   |4f 73 74 79 70 65 3a 20 6c 69 6e 75 78 0d 0a 53 65 72 76 65 72 3a 20 44 6f 63 6b 65 72 2f 32 30
                   |2e 31 30 2e 31 31 20 28 6c 69 6e 75 78 29 0d 0a 44 61 74 65 3a 20 54 68 75 2c 20 30 36 20 4f 63
                   |74 20 32 30 32 32 20 32 32 3a 34 39 3a 31 35 20 47 4d 54 0d 0a 43 6f 6e 74 65 6e 74 2d 4c 65 6e
                   |67 74 68 3a 20 31 31 33 38 0d 0a 0d 0a 5b 7b 22 49 64 22 3a 22 35 35 64 39 32 33 64 63 64 61 61
                   |62 37 62 64 61 39 31 39 34 62 30 39 36 33 31 32 33 65 38 63 34 34 64 30 62 35 33 36 64 62 32 35
                   |65 64 65 31 62 32 35 61 31 63 64 38 64 31 64 64 32 39 62 62 65 22 2c 22 4e 61 6d 65 73 22 3a 5b
                   |22 2f 66 61 73 74 2d 6f 70 65 6e 2d 63 61 73 68 22 5d 2c 22 49 6d 61 67 65 22 3a 22 70 6f 73 74
                   |67 72 65 73 22 2c 22 49 6d 61 67 65 49 44 22 3a 22 73 68 61 32 35 36 3a 31 31 33 33 61 39 63 64
                   |63 33 36 37 63 65 39 34 34 39 36 66 31 35 65 63 35 65 36 64 66 30 39 61 62 32 62 65 31 37 63 37
                   |63 65 61 30 36 64 64 61 36 31 62 31 39 36 66 66 64 65 32 64 66 39 62 30 22 2c 22 43 6f 6d 6d 61
                   |6e 64 22 3a 22 64 6f 63 6b 65 72 2d 65 6e 74 72 79 70 6f 69 6e 74 2e 73 68 20 70 6f 73 74 67 72
                   |65 73 22 2c 22 43 72 65 61 74 65 64 22 3a 31 36 36 34 37 38 30 36 39 35 2c 22 50 6f 72 74 73 22
                   |3a 5b 7b 22 49 50 22 3a 22 30 2e 30 2e 30 2e 30 22 2c 22 50 72 69 76 61 74 65 50 6f 72 74 22 3a
                   |35 34 33 32 2c 22 50 75 62 6c 69 63 50 6f 72 74 22 3a 35 34 33 32 2c 22 54 79 70 65 22 3a 22 74
                   |63 70 22 7d 2c 7b 22 49 50 22 3a 22 3a 3a 22 2c 22 50 72 69 76 61 74 65 50 6f 72 74 22 3a 35 34
                   |33 32 2c 22 50 75 62 6c 69 63 50 6f 72 74 22 3a 35 34 33 32 2c 22 54 79 70 65 22 3a 22 74 63 70
                   |22 7d 5d 2c 22 4c 61 62 65 6c 73 22 3a 7b 7d 2c 22 53 74 61 74 65 22 3a 22 72 75 6e 6e 69 6e 67
                   |22 2c 22 53 74 61 74 75 73 22 3a 22 55 70 20 33 20 64 61 79 73 22 2c 22 48 6f 73 74 43 6f 6e 66
                   |69 67 22 3a 7b 22 4e 65 74 77 6f 72 6b 4d 6f 64 65 22 3a 22 64 65 66 61 75 6c 74 22 7d 2c 22 4e
                   |65 74 77 6f 72 6b 53 65 74 74 69 6e 67 73 22 3a 7b 22 4e 65 74 77 6f 72 6b 73 22 3a 7b 22 62 72
                   |69 64 67 65 22 3a 7b 22 49 50 41 4d 43 6f 6e 66 69 67 22 3a 6e 75 6c 6c 2c 22 4c 69 6e 6b 73 22
                   |3a 6e 75 6c 6c 2c 22 41 6c 69 61 73 65 73 22 3a 6e 75 6c 6c 2c 22 4e 65 74 77 6f 72 6b 49 44 22
                   |3a 22 31 61 39 38 35 63 61 31 61 34 30 35 61 64 66 65 66 36 30 33 34 63 30 37 30 32 35 61 34 35
                   |38 35 65 63 64 34 33 64 38 66 36 62 34 61 30 32 65 37 61 37 62 34 31 37 63 36 63 39 66 66 64 38
                   |62 39 22 2c 22 45 6e 64 70 6f 69 6e 74 49 44 22 3a 22 35 35 36 31 30 35 31 32 31 39 63 34 61 64
                   |32 63 37 36 66 66 62 37 30 66 39 35 36 37 63 33 34 34 32 66 39 33 39 32 30 66 30 66 62 65 30 65
                   |36 30 30 38 38 35 37 32 38 35 61 34 32 38 31 64 31 33 22 2c 22 47 61 74 65 77 61 79 22 3a 22 31
                   |37 32 2e 31 37 2e 30 2e 31 22 2c 22 49 50 41 64 64 72 65 73 73 22 3a 22 31 37 32 2e 31 37 2e 30
                   |2e 32 22 2c 22 49 50 50 72 65 66 69 78 4c 65 6e 22 3a 31 36 2c 22 49 50 76 36 47 61 74 65 77 61
                   |79 22 3a 22 22 2c 22 47 6c 6f 62 61 6c 49 50 76 36 41 64 64 72 65 73 73 22 3a 22 22 2c 22 47 6c
                   |6f 62 61 6c 49 50 76 36 50 72 65 66 69 78 4c 65 6e 22 3a 30 2c 22 4d 61 63 41 64 64 72 65 73 73
                   |22 3a 22 30 32 3a 34 32 3a 61 63 3a 31 31 3a 30 30 3a 30 32 22 2c 22 44 72 69 76 65 72 4f 70 74
                   |73 22 3a 6e 75 6c 6c 7d 7d 7d 2c 22 4d 6f 75 6e 74 73 22 3a 5b 7b 22 54 79 70 65 22 3a 22 76 6f
                   |6c 75 6d 65 22 2c 22 4e 61 6d 65 22 3a 22 37 65 32 64 39 33 36 32 39 64 34 62 39 64 63 63 63 38
                   |64 64 36 62 64 34 32 61 32 65 39 63 66 62 63 65 31 38 64 34 66 33 37 30 37 65 65 35 39 64 63 61
                   |30 63 61 63 34 65 35 39 35 61 31 39 38 32 22 2c 22 53 6f 75 72 63 65 22 3a 22 22 2c 22 44 65 73
                   |74 69 6e 61 74 69 6f 6e 22 3a 22 2f 76 61 72 2f 6c 69 62 2f 70 6f 73 74 67 72 65 73 71 6c 2f 64
                   |61 74 61 22 2c 22 44 72 69 76 65 72 22 3a 22 6c 6f 63 61 6c 22 2c 22 4d 6f 64 65 22 3a 22 22 2c
                   |22 52 57 22 3a 74 72 75 65 2c 22 50 72 6f 70 61 67 61 74 69 6f 6e 22 3a 22 22 7d 5d 7d 5d 0a
                   |""".stripMargin
  lazy val bytes = bytesHex.split("\\r?\\n").map(_.trim).flatMap(_.split("\\s+")).map { bstr =>
    if( bstr.length!=2 ){
      throw new Error
    }else{
      Integer.parseInt(bstr.substring(0,1) + bstr.substring(1,2), 16).toByte
    }
  }.toList
  lazy val str = bytes.grouped(32).flatMap { bytes16 =>
    bytes16.map { byte =>
      ((byte & 0xF0) >> 4).toHexString + (byte & 0x0F).toHexString + " "
    }
  }.toList

  test("str") {
    //println(str.grouped(32).map(_.mkString).mkString("\n"))
    val dec = StandardCharsets.UTF_8.newDecoder()
    val bb = ByteBuffer.allocate(200)
    val strb = new StringBuilder
    bytes.grouped(bb.capacity()-82).foreach { byteBlock =>
      println(s"before ${byteBlock.size} bytes,        pos=${bb.position().toString.padTo(3,' ')} lim=${bb.limit()}")

      bb.put(byteBlock.toArray)
      println(s"after put ${byteBlock.size} bytes,     pos=${bb.position().toString.padTo(3,' ')} lim=${bb.limit()}")

      bb.flip()
      println(s"after flip ${byteBlock.size} bytes,    pos=${bb.position().toString.padTo(3,' ')} lim=${bb.limit()}")

      val cb = dec.decode(bb)
      println(s"after decode ${byteBlock.size} bytes,  pos=${bb.position().toString.padTo(3,' ')} lim=${bb.limit()}")

      bb.compact()
      println(s"after compact ${byteBlock.size} bytes, pos=${bb.position().toString.padTo(3,' ')} lim=${bb.limit()}")

      strb.append( cb.toString )
    }

    println( strb )
  }

  test("parse res fully"){
    val b2c = Decoder.Byte2Char(StandardCharsets.UTF_8.newDecoder())
    val c2l = Decoder.Char2Line()
    b2c.accept(bytes)
    c2l.accept(b2c.fetch)
    c2l.fetch.zipWithIndex.foreach { case(line,idx) =>
      println(s"[$idx] $line")
    }
  }

  test("sequence reader") {
    val reader = DecodeReader[Int,Int](
      SingleSupplier[Int](List(1,2,3,4,5,6,7,8,9)),
      Decoder.Buffer[Int]()
    )
    (0 until 15).foreach { _ =>
      println(reader.read)
    }
  }

  test("parse res stream") {
    val byteSupplier = SingleSupplier[Byte](bytes)

    val lineDec = Decoder.Char2Line().compose(
      Decoder.Byte2Char(StandardCharsets.UTF_8.newDecoder())
    )

    val reader = DecodeReader[Byte,String](byteSupplier, lineDec, sourceTimeout = 1L, readTimeout = 100L)
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
    println( reader.read )
  }

  test("parse http response") {
    HttpResponseReader(SingleSupplier[Byte](bytes)).read match {
      case Left(err) => println(s"error: $err")
      case Right(response) =>
        println(response.firstLine)
        println(response.headers.map {case(k,v) => s"$k: $v"}.mkString("\n"))
        println(s"body ${response.body.size} bytes")
        println(s"ok: ${response.isOk}")
        println(s"contentType: ${response.contentType}")
        println(s"charset: ${response.charset}")
        println(s"text: ${response.text}")

        response.text match {
          case None => ()
          case Some(text) => text.jsonAs[List[model.ContainerStatus]] match {
            case Left(value) => println(s"can't fetch json: $value")
            case Right(value) =>
              println(value)
          }
        }
    }
  }
}
