package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.property.SimpleStringProperty
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control.cell.TextFieldTableCell
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Accordion, TableColumn, TableView, TextArea, TitledPane}
import javafx.stage.Stage
import javafx.util.StringConverter
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.docker.model.Image
import xyz.cofe.lima.store.{AppConfig, ControllersHistory, History}

import java.util.concurrent.ConcurrentLinkedQueue

class PullImageController() {
  @FXML private var paramsTable:TableView[MutProp] = null
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
    paramsTable.getColumns.clear()

    val nameCol : TableColumn[MutProp,String] = {
      val tc = new TableColumn[MutProp,String]("Parameter")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.name)}
      tc
    }
    paramsTable.getColumns.add(nameCol)

    val valueCol : TableColumn[MutProp,String] = {
      val tc = new TableColumn[MutProp,String]("Value")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.reader())}
      tc
    }
    valueCol.setOnEditCommit({ ev => ev.getRowValue.writer(ev.getNewValue)})
    valueCol.setCellFactory(ev => {
      val tc = new TextFieldTableCell[MutProp,String](new StringConverter[String](){
        override def toString(`object`: String): String = `object`
        override def fromString(string: String): String = string
      })
      tc
    })
    paramsTable.getColumns.add(valueCol)
    paramsTable.setEditable(true)

    paramsTable.getItems.add(
      MutProp(
        "fromImage",
        ()=>fromImage.getOrElse(""),
        v=>{fromImage = if(v.trim.nonEmpty)Some(v.trim)else None }
      ))
    paramsTable.getItems.add(
      MutProp(
        "tag",
        ()=>tag.getOrElse(""),
        v=>{tag = if(v.trim.nonEmpty)Some(v.trim)else None }
      ))
    paramsTable.getItems.add(
      MutProp(
        "fromSrc",
        ()=>fromSrc.getOrElse(""),
        v=>{fromSrc = if(v.trim.nonEmpty)Some(v.trim)else None }
      ))
    paramsTable.getItems.add(
      MutProp(
        "repo",
        ()=>repo.getOrElse(""),
        v=>{repo = if(v.trim.nonEmpty)Some(v.trim)else None }
      ))
    paramsTable.getItems.add(
      MutProp(
        "message",
        ()=>message.getOrElse(""),
        v=>{message = if(v.trim.nonEmpty)Some(v.trim)else None }
      ))
    paramsTable.getItems.add(
      MutProp(
        "platform",
        ()=>platform.getOrElse(""),
        v=>{platform = if(v.trim.nonEmpty)Some(v.trim)else None }
      ))

    accordion.setExpandedPane(paramsTitledPane)

    history.last.foreach { e =>
      fromSrc = e.fromSrc
      fromImage = e.fromImage
      repo = e.repo
      tag = e.tag
      message = e.message
      platform = e.platform

      historyIndex = Some(history.size-1)
    }
  }

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
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
          paramsTable.refresh()
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
          paramsTable.refresh()
        }
      case _ =>
    }
  }

  def pull():Unit = {
    dockerClient.map(_.newClient).foreach { dc =>
      val p_fromImage = fromImage
      val p_fromSrc = fromSrc
      val p_repo = repo
      val p_tag = tag
      val p_message = message
      val p_platform = platform
      val th = new Thread("pull image") {
        override def run(): Unit = {
          history.add(Logger.ImageCreate(p_fromImage,p_fromSrc,p_repo,p_tag,p_message,p_platform))
          dc.imageCreate(p_fromImage,p_fromSrc,p_repo,p_tag,p_message,p_platform) { ev =>
            import xyz.cofe.lima.docker.model.ImagePullStatusEntry._
            ev.statusInfo match {
              case Some(PullingFrom(str)) =>
                log(s"pulling from $str")
              case Some(PullingFsLayer) =>
                log(s"pulling fs layer id=${ev.id}")
              case Some(Waiting) =>
                log(s"waiting id=${ev.id}")
              case Some(Downloading) =>
                log(s"downloading id=${ev.id} progress ${ev.progressDetail.map(d=>s"${d.current} / ${d.total}")}")
              case Some(VerifyingChecksum) =>
                log(s"VerifyingChecksum id=${ev.id}")
              case Some(DownloadComplete) =>
                log(s"DownloadComplete id=${ev.id}")
              case Some(Extracting) =>
                log(s"Extracting id=${ev.id} progress ${ev.progressDetail.map(d=>s"${d.current} / ${d.total}")}")
              case Some(s@PullComplete) =>
                log(s"PullComplete id=${ev.id}")
              case Some(Digest(str)) =>
                log(s"Digest $str")
              case Some(CommentedStatus(str)) =>
                log(s"CommentedStatus $str")
              case None => log("undefined")
              case _ => log("???")
            }
          }
        }
      }
      th.setDaemon(true)
      threads = th :: threads
      th.start()

      accordion.setExpandedPane(logsTitledPane)
    }
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
  def show(dc: DockerClient): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/pull-image.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[PullImageController]

    val stage = new Stage()
    controller.setDockerClient(dc)
    stage.setOnCloseRequest { _ => controller.closing() }

    stage.setTitle("Pull image")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()
  }
}
