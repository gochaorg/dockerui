package xyz.cofe.lima.docker.hub.model

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.{JsonReader, JsonWriter}
import xyz.cofe.lima.docker.model.ImageSearch

case class TagsRequest(
                        namespace:String,
                        repository:Option[String]
                      )
object TagsRequest {
  implicit val reader: JsonReader[TagsRequest] = jsonReader[TagsRequest]
  implicit val writer: JsonWriter[TagsRequest] = jsonWriter[TagsRequest]

  def apply(imageSearch: ImageSearch):Either[String,TagsRequest] = {
    if( imageSearch.name.isEmpty ){
      Left("name is empty")
    }else{
      val name = imageSearch.name.get
      if( name.contains("/") ){
        val ( ns, repo ) = name.split(":",2)
        Right(TagsRequest(ns,repo))
      }else{
        Right(TagsRequest(name,None))
      }
    }
  }
}

