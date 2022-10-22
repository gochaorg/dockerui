package xyz.cofe.lima.docker.http

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets

class HttpRequestTest extends AnyFunSuite {
  test("post sample") {
    val bytes = {
      HexDump.bytesFrom(
      """
        |  50 4f 53 54 20 2f 63 6f 6e 74 61 69 6e 65 72 73 2f 63 72 65 61 74 65 20 48 54 54 50 2f 31 2e 31
        |  0a 48 4f 53 54 3a 20 64 75 6d 6d 79 0a 55 73 65 72 2d 41 67 65 6e 74 3a 20 63 75 72 6c 2f 37 2e
        |  37 39 2e 31 0a 41 63 63 65 70 74 3a 20 2a 2f 2a 0a 43 6f 6e 74 65 6e 74 2d 4c 65 6e 67 74 68 3a
        |  20 35 33 0a 43 6f 6e 74 65 6e 74 2d 74 79 70 65 3a 20 61 70 70 6c 69 63 61 74 69 6f 6e 2f 6a 73
        |  6f 6e 0a 7b 22 49 6d 61 67 65 22 3a 22 61 6c 70 69 6e 65 22 2c 22 43 6d 64 22 3a 5b 22 65 63 68
        |  6f 22 2c 22 68 65 6c 6c 6f 22 5d 2c 22 4c 61 62 65 6c 73 22 3a 7b 7d 7d
        |""".stripMargin)
    }

    val str = new String(bytes,StandardCharsets.UTF_8)
    str.split("\\r?\\n").zipWithIndex.foreach { case(line,lidx) =>
      println(s"[$lidx] $line")
    }
  }

  test("post sample 2") {
    val req = HttpRequest("/containers/create").post().bodyText("{\"Image\":\"alpine\",\"Cmd\":[\"echo\",\"hello\"],\"Labels\":{}}")
    val bytes = req.toBytes

    val str = new String(bytes, StandardCharsets.UTF_8)
    str.split("\\r?\\n").zipWithIndex.foreach { case (line, lidx) =>
      println(s"[$lidx] $line")
    }
  }
}
