package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class Image( Id:String,
                  ParentId:String,
                  RepoTags:List[String],
                  RepoDigests:List[String],
                  Created:Int,
                  Size:Long,
                  SharedSize:Int,
                  VirtualSize:Long,
                  Labels: Map[String,String],
                  Containers: Int
                )
object Image {
  implicit val reader: JsonReader[Image] = jsonReader[Image]
}
