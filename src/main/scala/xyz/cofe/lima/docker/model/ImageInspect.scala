package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader

case class ImageInspect( Id:String,
                         RepoTags:List[String],
                         RepoDigests:List[String],
                         Parent:String,
                         Comment:String,
                         Created:String,
                         Container:String,
                         ContainerConfig:ContainerConfig,
                         DockerVersion:String,
                         Author:String,
                         Config:ContainerConfig,
                         Architecture:String,
                         Variant:Option[String],
                         Os:String,
                         OsVersion:Option[String],
                         Size:Long,
                         VirtualSize:Long,
                         GraphDriver:GraphDriver,
                         RootFs:RootFs,
                         Metadata:Map[String,String] )
object ImageInspect {
  implicit val reader: JsonReader[ImageInspect] = jsonReader[ImageInspect]
}

