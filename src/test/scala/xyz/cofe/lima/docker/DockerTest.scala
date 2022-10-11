package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.docker.http.{HttpLogger, SocketLogger}

class DockerTest extends AnyFunSuite {
  test("containers") {
    //implicit val log = HttpLogger.stdout
    DockerClient.unixSocket("/Users/g.kamnev/.colima/docker.sock").containers(all = true) match {
      case Left(err) => println(s"error $err")
      case Right(value) => value.foreach { c =>
        println(s"${c.Names} ${c.State}")
      }
    }
  }

  test("inspect") {
    //implicit val log = HttpLogger.stdout
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .containerInspect("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(ci) =>
        println(ci)
    }
  }

  test("processes inside") {
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
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
    implicit val log = HttpLogger.stdout
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .containerStart("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(_) => println(s"success")
    }
  }

  test("stop process") {
    implicit val log = HttpLogger.stdout
    DockerClient
      .unixSocket("/Users/g.kamnev/.colima/docker.sock")
      .containerStop("55d923dcdaab7bda9194b0963123e8c44d0b536db25ede1b25a1cd8d1dd29bbe") match {
      case Left(err) => println(s"error $err")
      case Right(_) => println(s"success")
    }
  }
}
