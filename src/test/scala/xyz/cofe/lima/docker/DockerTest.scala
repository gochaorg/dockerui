package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.docker.http.HttpResponseStream.Event
import xyz.cofe.lima.docker.http.{DelaiedReader, Decoder, HttpLogger, HttpRequest, HttpResponseStream, SocketLogger}
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.docker.model.CreateContainerRequest

import java.nio.charset.StandardCharsets

class DockerTest extends AnyFunSuite {
  val socket = "/var/run/docker.sock"
//  test("create container") {
//    implicit val httpLog = HttpLogger.stdout()
//    //implicit val socketLog = SocketLogger.stdout
//    DockerClient.unixSocket(
//      socket
//    ).copy(sourceTimeout = 1000L*30L, readTimeout = 1000L*30L).containerCreate(
//      CreateContainerRequest(
//        Image = "alpine",
//        Cmd = Some(List("echo", "hello"))
//      )
//    ) match {
//      case Left(err) => println(s"error $err")
//      case Right(value) =>
//        println(s"${value.Id}\n${value.Warnings}")
//    }
//  }

  test("containers") {
    //implicit val log = HttpLogger.stdout
    DockerClient.unixSocket(socket).containers(all = true) match {
      case Left(err) => println(s"error $err")
      case Right(value) => value.foreach { c =>
        println(s"${c.Names} ${c.State}")
      }
    }
  }

  test("images") {
    //implicit val log = HttpLogger.stdout()
    implicit val dlog = new Logger.JsonToWriter(System.out)
    DockerClient.unixSocket(socket).images() match {
      case Left(err) => println(s"error $err")
      case Right(value) => value.foreach { c =>
        println(s"${c}")
      }
    }
  }

  test("inspect") {
    //implicit val log = HttpLogger.stdout()
    implicit val log = Logger.JsonToWriter(System.out)
    DockerClient
      .unixSocket(socket)
      .containerInspect("53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4") match {
      case Left(err) =>
        println(s"error $err")
      case Right(ci) =>
        println(ci)
    }
  }

  test("processes inside") {
    DockerClient
      .unixSocket(socket)
      .containerProcesses("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(ci) =>
        println(ci)
    }
  }

  test("process log") {
    //implicit val log = HttpLogger.stdout
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .containerLogs("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(log) =>
        log.foreach { case (line) =>
          println(line)
        }
    }
  }

  test("start process") {
    implicit val log = HttpLogger.stdout()
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .containerStart("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(_) => println(s"success")
    }
  }

  test("stop process") {
    implicit val log = HttpLogger.stdout()
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .containerStop("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(_) => println(s"success")
    }
  }

//  test("stream reading") {
//    //val url = "http://localhost/v1.41/images/create?fromImage=redis&tag=sha256:2bd864580926b790a22c8b96fd74496fe87b3c59c0774fe144bab2788e78e676"
//
//    lazy val byte2charDecoder: Decoder.Byte2Char = Decoder.Byte2Char(StandardCharsets.UTF_8.newDecoder())
//    lazy val lineDecoder: Decoder[Byte, String, String] = Decoder.Char2Line().compose( byte2charDecoder )
//
//    DockerClient
//      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
//      .copy(readTimeout = -1,sourceTimeout = 1000L*60L*60L,cpuThrottling = 200L)
//      .stream(
//        HttpRequest("/images/create")
//          .queryString(Map(
//            "fromImage"->"redis",
//            "tag"->"sha256:2bd864580926b790a22c8b96fd74496fe87b3c59c0774fe144bab2788e78e676"
//          )).post()
//      ) { ev =>
//        ev match {
//          case Event.Error(_,string) => println(string)
//          case Event.FirstLine(_,string) => println(string)
//          case Event.Header(_,name, value) => println(s"$name: $value")
//          case Event.HeaderEnd(_) => println("header end")
//          case Event.Data(_,bytes) =>
//            lineDecoder.accept(bytes)
//            lineDecoder.fetch.foreach { println }
//          case Event.DataEnd(_) =>
//            println(lineDecoder.tail)
//            println("="*40)
//            println("END")
//        }
//        HttpResponseStream.Behavior.Continue
//      }
//  }

  test("image pull") {
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .imageCreate(
        fromImage = Some("redis"),
        tag = Some("sha256:2bd864580926b790a22c8b96fd74496fe87b3c59c0774fe144bab2788e78e676")
      ) { ev =>
        import xyz.cofe.lima.docker.model.ImagePullStatusEntry._
        ev.statusInfo match {
          case Some(PullingFrom(str)) =>
            println(s"pulling from $str")
          case Some(PullingFsLayer) =>
            println(s"pulling fs layer id=${ev.id}")
          case Some(Waiting) =>
            println(s"waiting id=${ev.id}")
          case Some(Downloading) =>
            println(s"downloading id=${ev.id} progress ${ev.progressDetail.map(d=>s"${d.current} / ${d.total}")}")
          case Some(VerifyingChecksum) =>
            println(s"VerifyingChecksum id=${ev.id}")
          case Some(DownloadComplete) =>
            println(s"DownloadComplete id=${ev.id}")
          case Some(Extracting) =>
            println(s"Extracting id=${ev.id} progress ${ev.progressDetail.map(d=>s"${d.current} / ${d.total}")}")
          case Some(s@PullComplete) =>
            println(s"PullComplete id=${ev.id}")
          case Some(Digest(str)) =>
            println(s"Digest $str")
          case Some(CommentedStatus(str)) =>
            println(s"CommentedStatus $str")
          case None => println("undefined")
          case _ => println("???")
        }
      }
  }
}
