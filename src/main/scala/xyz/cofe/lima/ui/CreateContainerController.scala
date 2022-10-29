package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.Observable
import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.fxml.FXML
import javafx.scene.control
import javafx.scene.control.cell.TextFieldTreeTableCell
import javafx.scene.control.{TreeItem, TreeTableColumn, TreeTableRow, TreeTableView}
import javafx.util.{Callback, StringConverter}
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.log.Logger.ContainerCreate
import xyz.cofe.lima.docker.model.CreateContainerRequest
import xyz.cofe.lima.store.{AppConfig, ControllersHistory, History}

class CreateContainerController {
  @FXML
  private var params : TreeTableView[MutProp] = null

  @FXML
  def initialize():Unit = {
    val nameCol = new TreeTableColumn[MutProp,String]("name")
    nameCol.setCellValueFactory(new Callback[TreeTableColumn.CellDataFeatures[MutProp,String],ObservableValue[String]] {
      override def call(param: TreeTableColumn.CellDataFeatures[MutProp, String]): ObservableValue[String] = {
        val str = param.getValue.getValue.name
        val strProp = new SimpleStringProperty(str)
        strProp.addListener(new ChangeListener[String] {
          override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
            println(s"change $oldValue => $newValue")
          }
        })
        strProp
      }
    })
    nameCol.setEditable(false)

    val valueCol = new TreeTableColumn[MutProp,String]("value")
    valueCol.setCellValueFactory(new Callback[TreeTableColumn.CellDataFeatures[MutProp,String],ObservableValue[String]] {
      override def call(param: TreeTableColumn.CellDataFeatures[MutProp, String]): ObservableValue[String] = {
        val str = param.getValue.getValue.reader()
        val strProp = new SimpleStringProperty(str)
        strProp.addListener(new ChangeListener[String] {
          override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
            println(s"change $oldValue => $newValue")
          }
        })
        strProp
      }
    })
    valueCol.setOnEditCommit(new EventHandler[TreeTableColumn.CellEditEvent[MutProp,String]](){
      override def handle(event: TreeTableColumn.CellEditEvent[MutProp, String]): Unit = {
        event.getRowValue.getValue.writer( event.getNewValue )
      }
    })
    valueCol.setEditable(true)

    params.getColumns.clear()
    params.getColumns.add(nameCol)
    params.getColumns.add(valueCol)
    params.setEditable(true)

    params.setRowFactory { table =>
      new TreeTableRow[MutProp]() {
        override def updateItem(item: MutProp, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          setEditable(true)
        }
      }
    }

    valueCol.setCellFactory { c =>
      new TextFieldTreeTableCell[MutProp,String](new StringConverter[String] {
        override def toString(`object`: String): String = `object`
        override def fromString(string: String): String = string
      }){
        override def startEdit(): Unit = {
          super.startEdit()
        }
      }
    }

    history.last.foreach { h =>
      request = h.createContainerRequest
      name = h.name
      platform = h.platform

      historyIndex = Some(history.size-1)
    }
  }

  var request:CreateContainerRequest = CreateContainerRequest("image name")
  var name:Option[String] = None
  var platform:Option[String] = None

  def history: History[ContainerCreate] = ControllersHistory.createContainerHistory
  var historyIndex:Option[Int] = None
  def historyPrev():Unit = {
    historyIndex match {
      case Some(idx) if idx > 0 =>
        history.get(idx-1).foreach { h =>
          request = h.createContainerRequest
          name = h.name
          platform = h.platform
          historyIndex = Some(idx-1)
          params.refresh()
        }
      case _ =>
    }
  }
  def historyNext():Unit = {
    historyIndex match {
      case Some(idx) if idx < (history.size-1) =>
        history.get(idx+1).foreach { h =>
          request = h.createContainerRequest
          name = h.name
          platform = h.platform
          historyIndex = Some(idx+1)
          params.refresh()
        }
      case _ =>
    }
  }

  def prepareEdit():Unit = {
    val root = new TreeItem(MutProp("CreateContainerController",()=>"",_=>()))

    val containerName = new TreeItem(MutProp("name",() => name.getOrElse(""), { v => name = if(v.trim.length<1) None else Some(v) }))
    root.getChildren.add(containerName)

    val platformName = new TreeItem(MutProp("platform",() => platform.getOrElse(""), { v => platform = if(v.trim.length<1) None else Some(v) }))
    root.getChildren.add(platformName)

    val imageName = new TreeItem(MutProp("image",() => request.Image, { v => request = request.copy(Image = v) }))
    root.getChildren.add(imageName)

    val userName = new TreeItem(MutProp("user",() => request.User.getOrElse(""), { v => request = request.copy(User = Some(v)) }))
    root.getChildren.add(userName)

    val attachStdin = new TreeItem(MutProp("attachStdin:Boolean",
      () => request.AttachStdin.map(_.toString).getOrElse(""),
      v => request = request.copy(AttachStdin = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(attachStdin)

    val attachStdout = new TreeItem(MutProp("attachStdout:Boolean",
      () => request.AttachStdout.map(_.toString).getOrElse(""),
      v => request = request.copy(AttachStdout = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(attachStdout)

    val attachStderr = new TreeItem(MutProp("attachStderr:Boolean",
      () => request.AttachStderr.map(_.toString).getOrElse(""),
      v => request = request.copy(AttachStderr = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(attachStderr)

    val tty = new TreeItem(MutProp("tty:Boolean",
      () => request.Tty.map(_.toString).getOrElse(""),
      v => request = request.copy(Tty = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(tty)

    val cmd = new TreeItem(MutProp("cmd",
      () => request.Tty.map(_.toString).getOrElse(""),
      _ => request = request))
    root.getChildren.add(cmd)

    def rebuildCmdRoot():Unit = {
      cmd.getChildren.clear()
      request.Cmd.foreach { cmdList =>
        cmdList.zipWithIndex.foreach { case (c, i) =>
          val ci = new TreeItem(MutProp(s"#$i",
            () => request.Cmd.map(cmdl => cmdl(i)).getOrElse(""),
            v => {
              request = request.copy(
                Cmd = if( v.nonEmpty ) {
                  request.Cmd.map(eCmdLine => eCmdLine.updated(i, v))
                }else{
                  request.Cmd.map(eCmdLine => eCmdLine.zipWithIndex.filter { case(_,ii) => ii!=i }.map(_._1))
                }
              )
              Platform.runLater(()=>{
                rebuildCmdRoot()
              })
            }
          ))
          cmd.getChildren.add(ci)
        }
      }
      val ci = new TreeItem(MutProp("new",()=>"",
        v => {
          if(v.trim.nonEmpty){
            request = request.copy(
              Cmd = request.Cmd.map(eCmdLine => eCmdLine ++ List(v)).orElse(Some(List(v)))
            )
            Platform.runLater(()=>{
              rebuildCmdRoot()
            })
          }
        }
      ))
      cmd.getChildren.add(ci)
    }
    rebuildCmdRoot()

    val hostConf = new TreeItem(MutProp("hostConfig", ()=>"", _ => ()));
    

    params.setShowRoot(false)
    params.setRoot(root)
  }

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
  }

  def createContainer():Unit = {
    dockerClient.foreach { dc =>
      request = request.copy(
        Cmd = request.Cmd.flatMap { cmdList =>
          if( cmdList.isEmpty ){
            None
          }else{
            Some(cmdList)
          }
        }
      )
      history.add(ContainerCreate(request, name, platform))
      dc.containerCreate(request, name, platform) match {
        case Left(err) =>
        case Right(resp) => println(resp)
      }
    }
  }
}
