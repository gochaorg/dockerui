package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite

import java.net.URI
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}

class DockerHubTest extends AnyFunSuite {
  test("test") {
    val req = HttpRequest
      .newBuilder(new URI("https://hub.docker.com/v2/repositories/ansible/?page_size=3"))
      .GET()
      .build()

    val client = HttpClient.newHttpClient()

    val resp = client.send(req, BodyHandlers.ofString())
    println(resp.body())
  }

  test("images") {
    DockerHub().images("ubuntu",Some(100)) match {
      case Left(value) => println(value)
      case Right(value) =>
        value.results.map { im => s"${im.name} ${im.namespace}" }.foreach(println)
    }
  }

  test("tags") {
    DockerHub().tags("ubuntu",Some(100)) match {
      case Left(value) => println(value)
      case Right(value) =>
        value.results.map { im => s"${im.name}\n"+
          (im.images.map(i=>s"  ${i.architecture} ${i.os} ${i.digest}").mkString("\n"))
        }.foreach(println)
    }
  }
}
