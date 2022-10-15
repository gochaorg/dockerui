package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

case class ImagePullStatusEntry(
  status:String,
  id:Option[String],
  progressDetail:Option[ImagePullStatusEntryProgress],
  progress: Option[String],
) {
  import ImagePullStatusEntry._
  lazy val statusInfo:Option[Status] = {
    if( status.startsWith("Pulling from ") ) {
      Some(PullingFrom(status.substring("Pulling from ".length)))
    }else if( status=="Pulling fs layer" ){
      Some(PullingFsLayer)
    }else if( status=="Waiting" ){
      Some(Waiting)
    }else if( status=="Downloading" ){
      Some(Downloading)
    }else if( status=="Verifying Checksum" ){
      Some(VerifyingChecksum)
    }else if( status=="Download complete" ){
      Some(DownloadComplete)
    }else if( status=="Extracting" ){
      Some(Extracting)
    }else if( status=="Pull complete" ){
      Some(PullComplete)
    }else if( status.startsWith("Digest:") ){
      Some(Digest(status.substring("Digest:".length).trim))
    }else if( status.startsWith("Status:") ){
      Some(Digest(status.substring("Status:".length).trim))
    }else{
      None
    }
  }
}

object ImagePullStatusEntry {
  implicit val reader: JsonReader[ImagePullStatusEntry] = jsonReader[ImagePullStatusEntry]
  implicit val writer: JsonWriter[ImagePullStatusEntry] = jsonWriter[ImagePullStatusEntry]

  sealed trait Status
  case class PullingFrom(from:String) extends Status
  case object PullingFsLayer extends Status
  case object Waiting extends Status
  case object Downloading extends Status
  case object VerifyingChecksum extends Status
  case object DownloadComplete extends Status
  case object Extracting extends Status
  case object PullComplete extends Status
  case class Digest(digest:String) extends Status
  case class CommentedStatus(comment:String) extends Status
}

case class ImagePullStatusEntryProgress(
  current: Option[Long],
  total: Option[Long],
)
object ImagePullStatusEntryProgress {
  implicit val reader: JsonReader[ImagePullStatusEntryProgress] = jsonReader[ImagePullStatusEntryProgress]
  implicit val writer: JsonWriter[ImagePullStatusEntryProgress] = jsonWriter[ImagePullStatusEntryProgress]
}

