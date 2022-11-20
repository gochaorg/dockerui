package xyz.cofe.lima.docker.hub

import model._
import tethys.JsonReader

import java.net.URI
import java.net.http.HttpClient
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.http.QueryStringBuilder
import xyz.cofe.lima.thread.ThreadID

import java.util.concurrent.atomic.AtomicLong

/**
 * https://www.baeldung.com/ops/docker-registry-api-list-images-tags
 */
trait DockerHubClient {
  /**
   * GET https://hub.docker.com/v2/repositories/${namespace}?page_size=${page_size}&page=${pageNum}
   * @param namespace
   * @param pageSize
   * @param pageNum
   * @return
   */
  def images(namespace:String,pageSize:Option[Int]=None,pageNum:Option[Int]=None):Either[String,Images]

  /**
   * GET https://hub.docker.com/v2/repositories/library/${namespace}/tags?page_size=${page_size}&page=${pageNum}
   * @param namespace
   * @param pageSize
   * @param pageNum
   * @return
   */
  def tags(namespace:String,pageSize:Option[Int],pageNum:Option[Int]):Either[String,Tags]

  /**
    * https://docs.docker.com/docker-hub/api/latest/#tag/repositories
    * GET https://hub.docker.com/v2/namespaces/{namespace}/repositories/{repository}/tags?page_size=${page_size}&page=${pageNum}
    */
  def tags(namespace:String, repository:String, pageSize:Option[Int],pageNum:Option[Int]):Either[String,Tags]

  def tags(request: TagsRequest):Either[String,Tags]
}

object DockerHubClient {
  def apply(client:HttpClient):Java11Client = new Java11Client(client)

  private val reqIdSeq = new AtomicLong()

  class Java11Client(client:HttpClient) extends DockerHubClient {
    private def uri(path:String,queryString:Option[String]):URI =
      new URI("https","hub.docker.com",path,queryString.orNull,null)

    private def getRequest(path:String,queryString:Option[String]):java.net.http.HttpRequest =
      java.net.http.HttpRequest.newBuilder(uri(path,queryString))
        .GET()
        .build()

    private def getJson[A:JsonReader](request: java.net.http.HttpRequest):Either[String,A] = {
      val reqId = reqIdSeq.incrementAndGet()
      println(s"getJson #$reqId ${request.uri()} thread "+ThreadID.current)
      try {
        val response = client.send(request, java.net.http.HttpResponse.BodyHandlers.ofString())
        println(s"response #$reqId for ${request.uri()} status ${response.statusCode()}"+ThreadID.current)
        if (response.statusCode() >= 200 && response.statusCode() < 300) {
          response.body().jsonAs[A].left.map(_.getMessage)
        } else {
          Left(s"status code not valid for ${request.uri()} ")
        }
      } catch {
        case err:Throwable =>
          println(s"accept error $err for ${request.uri()}")
          Left(err.getMessage)
      }
    }

    def images(namespace: String, pageSize: Option[Int], pageNum: Option[Int]):Either[String,Images] =
      getJson[Images](
        getRequest(s"/v2/repositories/$namespace",
          QueryStringBuilder().queryString(
            "page_size"->pageSize,
            "page"->pageNum
          )
        )
      )

    def tags(namespace: String, pageSize: Option[Int], pageNum: Option[Int]):Either[String,Tags] =
      getJson[Tags](
        getRequest(s"/v2/repositories/library/$namespace/tags",
          QueryStringBuilder().queryString(
            "page_size" -> pageSize,
            "page" -> pageNum
          )
        )
      )
    def tags(namespace: String, repository: String, pageSize: Option[Int], pageNum: Option[Int]):Either[String,Tags] =
      getJson[Tags](
        getRequest(s"/v2/namespaces/$namespace/repositories/$repository/tags",
          QueryStringBuilder().queryString(
            "page_size" -> pageSize,
            "page" -> pageNum
          )
        )
      )

    override def tags(request: TagsRequest):Either[String,Tags] = {
      if( request.repository.isDefined ){
        tags( request.namespace, None, None )
      }else{
        tags( request.namespace, request.repository.get, None, None )
      }
    }
  }
}