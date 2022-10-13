package xyz.cofe.lima.docker

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.docker.DockerHub.{Images, Tags}

import java.net.{URI, URLEncoder}
import java.net.http.HttpResponse.BodyHandlers
import java.net.http.{HttpClient, HttpRequest, HttpResponse}
import java.nio.charset.StandardCharsets

trait DockerHub {
  import DockerHub._
  def images(namespace:String,pageSize:Option[Int]=None,pageNum:Option[Int]=None):Either[String,Images]
  def tags(namespace:String,pageSize:Option[Int]=None,pageNum:Option[Int]=None):Either[String,Tags]
}

object DockerHub {
  case class Images(
                     count:Int,
                     next:Option[String],
                     previous:Option[String],
                     results:List[Image]
                   )
  object Images {
    implicit val reader:JsonReader[Images] = jsonReader[Images]
  }

  case class Image(
                    name:String,
                    namespace:String,
                    repository_type:Option[String],
                    status:Int,
                    is_private:Boolean,
                    star_count:Int,
                    pull_count:Long,
                    last_updated:String,
                    date_registered:String,
                    affiliation:String,
                    media_types:List[Option[String]]
                  )
  object Image {
    implicit val reader:JsonReader[Image] = jsonReader[Image]
  }

  case class Tags(count:Int,next:Option[String],previous:Option[String],results:List[Tag])
  object Tags { implicit val reader:JsonReader[Tags] = jsonReader[Tags] }

  case class Tag(
                  creator:Int,
                  id:Int,
                  last_updated:String,
                  last_updater:Option[Int],
                  last_updater_username:Option[String],
                  name:String,
                  repository:Int,
                  full_size:Long,
                  v2:Boolean,
                  tag_status:String,
                  tag_last_pulled:String,
                  tag_last_pushed:String,
                  media_type:Option[String],
                  digest:Option[String],
                  images:List[ImageTag]
                )
  object Tag { implicit val reader:JsonReader[Tag] = jsonReader[Tag] }

  case class ImageTag(
                       architecture:String,
                       features:String,
                       variant:Option[String],
                       digest:Option[String],
                       os:String,
                       os_features:String,
                       os_version:Option[String],
                       size:Long,
                       status:String,
                       last_pulled:String,
                       last_pushed:String)
  object ImageTag { implicit val reader:JsonReader[ImageTag] = jsonReader[ImageTag] }

  def apply():DockerHub = {
    DockerHubImpl(HttpClient.newHttpClient())
  }
}

case class DockerHubImpl(client:HttpClient) extends DockerHub {
  def images(namespace:String,pageSize:Option[Int]=None,pageNum:Option[Int]=None):Either[String,Images] = {
    val qs = Map("page_size"->pageSize,"page"->pageNum)
      .filter(_._2.isDefined)
      .map { case(k,v) => k->v.get.toString }
      .map { case(k,v) => k+"="+v }
      .foldLeft(""){ case (sum,kv) => sum match {
        case "" => kv
        case _ => sum+"&"+kv
      }} match {
      case "" => ""
      case str:String => "?"+str
    }

    val req = HttpRequest
      .newBuilder(new URI(s"https://hub.docker.com/v2/repositories/${
        namespace
      }/"+qs))
      .GET()
      .build()

    val resp = client.send(req,BodyHandlers.ofString())
    if( !(resp.statusCode()>=200 && resp.statusCode()<300) ){
      Left(s"response code ${resp.statusCode()}")
    }else{
      import tethys._
      import tethys.jackson._
      resp.body().jsonAs[DockerHub.Images].left.map(err => err.getMessage)
    }
  }
  def tags(namespace:String,pageSize:Option[Int]=None,pageNum:Option[Int]=None):Either[String,Tags] = {
    val qs = Map("page_size"->pageSize,"page"->pageNum)
      .filter(_._2.isDefined)
      .map { case(k,v) => k->v.get.toString }
      .map { case(k,v) => k+"="+v }
      .foldLeft(""){ case (sum,kv) => sum match {
        case "" => kv
        case _ => sum+"&"+kv
      }} match {
      case "" => ""
      case str:String => "?"+str
    }

    val req = HttpRequest
      .newBuilder(new URI(s"https://hub.docker.com/v2/repositories/library/${
        namespace
      }/tags"+qs))
      .GET()
      .build()

    val resp = client.send(req,BodyHandlers.ofString())
    if( !(resp.statusCode()>=200 && resp.statusCode()<300) ){
      Left(s"response code ${resp.statusCode()}")
    }else{
      import tethys._
      import tethys.jackson._
      resp.body().jsonAs[DockerHub.Tags].left.map(err => err.getMessage)
    }
  }
}