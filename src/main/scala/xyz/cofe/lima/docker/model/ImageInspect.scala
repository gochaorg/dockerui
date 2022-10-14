package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import xyz.cofe.lima.{TreeShow, TreeShowDerivation}

case class ImageInspect( Id:String,
                         RepoTags:List[String],
                         RepoDigests:List[String],
                         Parent:String,
                         Comment:String,
                         Created:String,
                         Container:String,
                         ContainerConfig:Option[ContainerConfig],
                         DockerVersion:Option[String],
                         Author:Option[String],
                         Config:Option[ContainerConfig],
                         Architecture:Option[String],
                         Variant:Option[String],
                         Os:Option[String],
                         OsVersion:Option[String],
                         Size:Option[Long],
                         VirtualSize:Option[Long],
                         GraphDriver:Option[GraphDriver],
                         RootFs:Option[RootFs],
                         Metadata:Map[String,String] )
object ImageInspect {
  implicit val reader: JsonReader[ImageInspect] = jsonReader[ImageInspect]
  implicit val writer: JsonWriter[ImageInspect] = jsonWriter[ImageInspect]
  implicit val showTree: TreeShow[ImageInspect] = TreeShowDerivation.gen[ImageInspect]
}

