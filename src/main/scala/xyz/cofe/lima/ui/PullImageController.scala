package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.property.SimpleStringProperty
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.cell.TextFieldTableCell
import javafx.scene.control._
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import javafx.util.StringConverter
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.docker.model.{ImagePullHttpStatus, ImagePullStatusEntry}
import xyz.cofe.lima.store.ControllersHistory

import java.util.concurrent.ConcurrentLinkedQueue

class PullImageController() {
  @FXML private var params1:TableView[MutProp] = null
  @FXML private var paramsTitledPane:TitledPane = null
  @FXML private var logsTitledPane:TitledPane = null
  @FXML private var logs:TextArea = null
  @FXML private var accordion:Accordion = null

  var fromImage:Option[String] = None
  var fromSrc:Option[String] = None
  var repo:Option[String] = None
  var tag:Option[String] = None
  var message:Option[String] = None
  var platform:Option[String] = None

  private def history = ControllersHistory.imageCreateHistory

  @FXML def initialize():Unit = {
    init()
  }

  private def init():Unit = {
    params1.getColumns.clear()

    val nameCol: TableColumn[MutProp, String] = {
      val tc = new TableColumn[MutProp, String]("Parameter")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.name) }
      tc
    }
    params1.getColumns.add(nameCol)

    val valueCol: TableColumn[MutProp, String] = {
      val tc = new TableColumn[MutProp, String]("Value")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.reader()) }
      tc
    }
    valueCol.setOnEditCommit({ ev => ev.getRowValue.writer(ev.getNewValue) })
    valueCol.setCellFactory(ev => {
      val tc = new TextFieldTableCell[MutProp, String](new StringConverter[String]() {
        override def toString(`object`: String): String = `object`

        override def fromString(string: String): String = string
      })
      tc
    })
    params1.getColumns.add(valueCol)
    params1.setEditable(true)

    params1.getItems.add(
      MutProp(
        "fromImage",
        () => fromImage.getOrElse(""),
        v => {
          fromImage = if (v.trim.nonEmpty) Some(v.trim) else None
        }
      ))
    params1.getItems.add(
      MutProp(
        "tag",
        () => tag.getOrElse(""),
        v => {
          tag = if (v.trim.nonEmpty) Some(v.trim) else None
        }
      ))
    params1.getItems.add(
      MutProp(
        "fromSrc",
        () => fromSrc.getOrElse(""),
        v => {
          fromSrc = if (v.trim.nonEmpty) Some(v.trim) else None
        }
      ))
    params1.getItems.add(
      MutProp(
        "repo",
        () => repo.getOrElse(""),
        v => {
          repo = if (v.trim.nonEmpty) Some(v.trim) else None
        }
      ))
    params1.getItems.add(
      MutProp(
        "message",
        () => message.getOrElse(""),
        v => {
          message = if (v.trim.nonEmpty) Some(v.trim) else None
        }
      ))
    params1.getItems.add(
      MutProp(
        "platform",
        () => platform.getOrElse(""),
        v => {
          platform = if (v.trim.nonEmpty) Some(v.trim) else None
        }
      ))

    accordion.setExpandedPane(paramsTitledPane)

    history.last.foreach { e =>
      fromSrc = e.fromSrc
      fromImage = e.fromImage
      repo = e.repo
      tag = e.tag
      message = e.message
      platform = e.platform

      historyIndex = Some(history.size - 1)
    }
  }

  private var threads = List[Thread]()

  private var historyIndex:Option[Int] = None
  def historyPrev():Unit = {
    historyIndex match {
      case Some(idx) if idx>0 =>
        history.get(idx-1).foreach { e =>
          fromSrc = e.fromSrc
          fromImage = e.fromImage
          repo = e.repo
          tag = e.tag
          message = e.message
          platform = e.platform

          historyIndex = Some(idx-1)
          params1.refresh()
        }
      case _ =>
    }
  }
  def historyNext():Unit = {
    historyIndex match {
      case Some(idx) if idx<(history.size-1) =>
        history.get(idx + 1).foreach { e =>
          fromSrc = e.fromSrc
          fromImage = e.fromImage
          repo = e.repo
          tag = e.tag
          message = e.message
          platform = e.platform

          historyIndex = Some(idx + 1)
          params1.refresh()
        }
      case _ =>
    }
  }

  def pull(): Unit = {
    val p_fromImage = fromImage
    val p_fromSrc = fromSrc
    val p_repo = repo
    val p_tag = tag
    val p_message = message
    val p_platform = platform

    history.add(Logger.ImageCreate(p_fromImage, p_fromSrc, p_repo, p_tag, p_message, p_platform))

    DockerClientPool.submit { dc =>
      dc.imageCreate(p_fromImage, p_fromSrc, p_repo, p_tag, p_message, p_platform) { ev =>
        import xyz.cofe.lima.docker.model.ImagePullStatusEntry._
        ev match {
          case ev:ImagePullStatusEntry =>
            ev.statusInfo match {
              case Some(PullingFrom(str)) =>
                //log("ss")
                log(s"pulling from $str")
              case Some(PullingFsLayer) =>
                log(s"pulling fs layer id=${ev.id}")
              case Some(Waiting) =>
                log(s"waiting id=${ev.id}")
              case Some(Downloading) =>
                log(s"downloading id=${ev.id} progress ${ev.progressDetail.map(d => s"${d.current} / ${d.total}")}")
              case Some(VerifyingChecksum) =>
                log(s"VerifyingChecksum id=${ev.id}")
              case Some(DownloadComplete) =>
                log(s"DownloadComplete id=${ev.id}")
              case Some(Extracting) =>
                log(s"Extracting id=${ev.id} progress ${ev.progressDetail.map(d => s"${d.current} / ${d.total}")}")
              case Some(s@PullComplete) =>
                log(s"PullComplete id=${ev.id}")
              case Some(Digest(str)) =>
                log(s"Digest $str")
              case Some(CommentedStatus(str)) =>
                log(s"CommentedStatus $str")
              case None => log("undefined")
              case _ => log("???")
            }
          case ImagePullHttpStatus(code, message) =>
            log(s"http response $code $message")
        }
      }
    }

    accordion.setExpandedPane(logsTitledPane)
  }

  private val messageQueue = new ConcurrentLinkedQueue[String]()
  private def log(message:String):Unit = {
    messageQueue.add(message)
    flush()
  }
  private def flush():Unit = {
    Platform.runLater(()=>{
      var stop=false
      while(!stop){
        val msg = messageQueue.poll()
        if( msg!=null ){
          logs.insertText(logs.getText.length,msg+"\n")
        }else{
          stop = true
        }
      }
    })
  }

  def closing():Unit = {
    threads.filter(_.isAlive).foreach { th =>
      while(th.isAlive) {
        th.interrupt()
      }
    }
  }
}

object PullImageController {
  def show: Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/pull-image.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[PullImageController]

    val stage = new Stage()
    stage.setOnCloseRequest { _ => controller.closing() }

    stage.setTitle("Pull image")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()
  }

  def show(imageCreate:Logger.ImageCreate): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/pull-image.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[PullImageController]

    val stage = new Stage()
    stage.setOnCloseRequest { _ => controller.closing() }

    stage.setTitle("Pull image")

    controller.fromImage = imageCreate.fromImage
    controller.fromSrc = imageCreate.fromSrc
    controller.repo = imageCreate.repo
    controller.tag = imageCreate.tag
    controller.message = imageCreate.message
    controller.platform = imageCreate.platform

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()
  }
}
