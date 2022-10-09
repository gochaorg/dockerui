package xyz.cofe.lima.docker

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite

import java.io.ByteArrayOutputStream
import java.net.UnixDomainSocketAddress
import java.nio.ByteBuffer
import java.nio.channels.SocketChannel
import java.nio.charset.StandardCharsets
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.{HttpResponseReader, SocketChannelSupplier}

class UnixSocketSpec extends AnyFunSuite {
  // http://dummy/containers/json
  // curl -s --unix-socket ~/.colima/docker.sock http://dummy/containers/json | jq '.'
  test("read socket") {
    val sock = SocketChannel.open(UnixDomainSocketAddress.of("/Users/g.kamnev/.colima/docker.sock"))
    sock.configureBlocking(false)

    val query =
      "GET /containers/json HTTP/1.1\n"+
      "HOST: dummy\n"+
      "User-Agent: curl/7.79.1\n"+
      "Accept: */*\n"+
      "\n"

    val sendBytes = query.getBytes(StandardCharsets.UTF_8);
    val sendBB = ByteBuffer.allocate(1024)
    sendBB.clear()
    sendBB.put(sendBytes)
    sendBB.flip()
    println(sendBB.capacity())
    println(sendBB.position())
    println(sendBB.limit())
    val writed = sock.write(sendBB)
    println(s"writed $writed")

    val responseByteBuffer = ByteBuffer.allocate(500)
    var stop = false
    val readedBytes = new ByteArrayOutputStream()

    Thread.sleep(500)

    while(!stop) {
      val readed = sock.read(responseByteBuffer)
      println(s"read $readed")
      if( readed<=0 ){
        stop = true
      }else{
        responseByteBuffer.flip()
        val arr = new Array[Byte](responseByteBuffer.limit() - responseByteBuffer.position())
        responseByteBuffer.get(arr)
        readedBytes.write(arr)
        responseByteBuffer.clear()
      }
    }

    val bytes = readedBytes.toByteArray
    println(s"${bytes.size} bytes")

    val str = new String(bytes,0,bytes.size,StandardCharsets.UTF_8)

    println("-"*40)

    bytes.grouped(32).foreach { bytes16 =>
      bytes16.foreach { byte =>
        print( ((byte & 0xF0) >> 4).toHexString + (byte & 0x0F).toHexString + " " )
      }
      println()
    }

    println("="*40)
    println(str)
  }

  test("containers") {
    val sock = SocketChannel.open(UnixDomainSocketAddress.of("/Users/g.kamnev/.colima/docker.sock"))
    sock.configureBlocking(false)

    val query =
      "GET /containers/json HTTP/1.1\n"+
        "HOST: dummy\n"+
        "User-Agent: curl/7.79.1\n"+
        "Accept: */*\n"+
        "\n"

    val sendBytes = query.getBytes(StandardCharsets.UTF_8);
    val sendBB = ByteBuffer.allocate(1024)
    sendBB.clear()
    sendBB.put(sendBytes)
    sendBB.flip()
    println(sendBB.capacity())
    println(sendBB.position())
    println(sendBB.limit())

    val writed = sock.write(sendBB)
    println(s"writed $writed")

    val socketChannelSupplier = SocketChannelSupplier(sock)
    HttpResponseReader(socketChannelSupplier).read match {
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
            case Right(containersInfo) =>
              containersInfo.foreach { ci =>
                println(
                  s"""
                     |${ci.Names} ${ci.State} ${ci.Id}
                     |""".stripMargin.trim)
              }
          }
        }
    }
  }
}
