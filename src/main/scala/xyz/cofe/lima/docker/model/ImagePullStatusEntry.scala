package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import xyz.cofe.lima.store.json._

sealed trait ImagePullStatus

case class ImagePullStatusEntry(
  status:String,
  id:Option[String],
  progressDetail:Option[ImagePullStatusEntryProgress],
  progress: Option[String],
) extends ImagePullStatus
{
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
  implicit val writer: JsonWriter[ImagePullStatusEntry] = classWriter[ImagePullStatusEntry] ++ jsonWriter[ImagePullStatusEntry]

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
  implicit val writer: JsonWriter[ImagePullStatusEntryProgress] = classWriter[ImagePullStatusEntryProgress] ++ jsonWriter[ImagePullStatusEntryProgress]
}

case class ImagePullHttpStatus(
  code: Int,
  message: Option[String]
) extends ImagePullStatus
object ImagePullHttpStatus {
  implicit val reader: JsonReader[ImagePullHttpStatus] = jsonReader[ImagePullHttpStatus]
  implicit val writer: JsonWriter[ImagePullHttpStatus] = classWriter[ImagePullHttpStatus] ++ jsonWriter[ImagePullHttpStatus]
}

object ImagePullStatus {
  implicit val writer : JsonWriter[ImagePullStatus] = new JsonWriter[ImagePullStatus] {
    def write(value: ImagePullStatus, tokenWriter: TokenWriter): Unit = {
      value match {
        case e : ImagePullStatusEntry =>
          ImagePullStatusEntry.writer.write(e, tokenWriter)
        case e : ImagePullHttpStatus => ???
          ImagePullHttpStatus.writer.write(e, tokenWriter)
      }
    }
  }
  implicit val reader : JsonReader[ImagePullStatus] = JsonReader.builder.addField[String]("_type")
    .selectReader[ImagePullStatus] {
      case "ImagePullStatusEntry" => ImagePullStatusEntry.reader
      case "ImagePullHttpStatus" => ImagePullHttpStatus.reader
    }
}