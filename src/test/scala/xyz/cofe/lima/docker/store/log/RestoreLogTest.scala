package xyz.cofe.lima.docker.store.log

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.docker.http.{Decoder, HttpLogger}
import xyz.cofe.lima.fs.syntax._

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import java.util
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.HttpLogger.LogEvent

class RestoreLogTest extends AnyFunSuite {
  val testFileOpt:Option[Path] = {
    val f = Path.of(".dockerui/log/http/2022-10/30/15-17-37.stream.json")
    if( f.exists.getOrElse(false) ){
      Some(f)
    }else{
      None
    }
  }

  test("try read http log 1") {
    if(testFileOpt.isEmpty){
      println("test file not found")
    }else{
      val testFile = testFileOpt.get
      val decoder = Decoder.Char2JsonEntry()
      testFile.reader(StandardCharsets.UTF_8).map { reader =>

        val cbuff = new Array[Char](1024*8)
        var stop = false
        while(!stop){
          val reads = reader.read(cbuff)
          if( reads<=0 ){
            stop = true
          }else{
            decoder.accept(util.Arrays.copyOf(cbuff,reads))
            decoder.fetch.foreach { jsonStr =>
              jsonStr.jsonAs[HttpLogger.LogEvent] match {
                case Left(value) => println(s"fail restore $value")
                case Right(value) => value match {
                  case LogEvent.Send(request, threadId, time) => println(
                    s"""request id=${request.id} ${request.path}
                       |""".stripMargin)
                  case LogEvent.Receive(response, threadId, time) => println(
                    s"""response pid=${response.pid} ${response.code}
                       |  body: ${response.text}
                       |""".stripMargin)
                  case LogEvent.Event(streamEvent, threadId, time) => ()
                  case LogEvent.Error(message, threadId, time) => ()
                }
              }
            }
          }
        }

        reader.close()
      }
    }
  }
}
