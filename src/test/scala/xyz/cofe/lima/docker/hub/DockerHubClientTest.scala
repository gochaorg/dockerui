package xyz.cofe.lima.docker.hub

import org.scalatest.funsuite.AnyFunSuite

import java.net.http.HttpClient

class DockerHubClientTest extends AnyFunSuite {
  test("tags by namespace") {
    println("search ubuntu")
    val res = DockerHubClient(HttpClient.newBuilder().build()).tags("ubuntu",None,None)
    if( res.isLeft )println(res)
    res.foreach( tags => {
      tags.results.foreach { tag =>
        println(s"${tag.name} ${tag.full_size}")
        tag.images.foreach { img =>
          println(s"  ${img.architecture} ${img.os} ${img.status}")
        }
      }
    })
  }

  test("tags by namespace + name") {
    println("search ubuntu")
    val res = DockerHubClient(HttpClient.newBuilder().build()).tags("dokken", "debian-9", None, None)
    if( res.isLeft )println(res)
    res.foreach(tags => {
      tags.results.foreach { tag =>
        println(s"${tag.name} ${tag.full_size}")
        tag.images.foreach { img =>
          println(s"  ${img.architecture} ${img.os} ${img.status}")
        }
      }
    })
  }
}
