package xyz.cofe.lima.docker.http

import org.scalatest.funsuite.AnyFunSuite

class DecoderTest extends AnyFunSuite {
  test("json entries") {
    val decoder = Decoder.Char2JsonEntry()
    decoder.accept(
      """
        |{"status":"Pulling from library/redis","id":"docker.io/library/redis@sha256:2bd864580926b790a22c8b96fd74496fe87b3c59c0774fe144bab2788e78e676"}
        |{"status":"Pulling fs layer","progressDetail":{},"id":"bd159e379b3b"}
        |{"status":"Pulling fs layer","progressDetail":{},"id":"729b630784ac"}
        |{"status":"Pulling fs layer","progressDetail":{},"id":"065c77bf222a"}
        |{"status":"Pulling fs layer","progressDetail":{},"id":"961784053f68"}
        |{"status":"Pulling fs layer","progressDetail":{},"id":"b42f16846808"}
        |{"status":"Pulling fs layer","progressDetail":{},"id":"0f1fa5bb0033"}
        |{"status":"Verifying Checksum","progressDetail":{},"id":"065c77bf222a"}
        |{"status":"Download complete","progressDetail":{},"id":"065c77bf222a"}
        |""".stripMargin)
    decoder.fetch.zipWithIndex.foreach { case(entry,idx) =>
      println(s"[$idx] ${entry.trim}")
    }
  }
}
