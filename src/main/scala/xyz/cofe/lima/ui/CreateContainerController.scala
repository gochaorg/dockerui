package xyz.cofe.lima.ui

import javafx.beans.Observable
import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.fxml.FXML
import javafx.scene.control.cell.TextFieldTreeTableCell
import javafx.scene.control.{TreeItem, TreeTableColumn, TreeTableRow, TreeTableView}
import javafx.util.{Callback, StringConverter}
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.model.CreateContainerRequest

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
  }

  var request:CreateContainerRequest = CreateContainerRequest("image name")
  var name:Option[String] = None
  var platform:Option[String] = None

  def prepareEdit():Unit = {
    val containerName = new TreeItem(MutProp("name",() => name.getOrElse(""), { v => name = if(v.trim.length<1) None else Some(v) }))
    val platformName = new TreeItem(MutProp("platform",() => platform.getOrElse(""), { v => platform = if(v.trim.length<1) None else Some(v) }))

    val imageName = new TreeItem(MutProp("image",() => request.Image, { v => request = request.copy(Image = v) }))
    val userName = new TreeItem(MutProp("user",() => request.User.getOrElse(""), { v => request = request.copy(User = Some(v)) }))

    val root = new TreeItem(MutProp("CreateContainerController",()=>"",_=>()))
    root.getChildren.add(containerName)
    root.getChildren.add(platformName)
    root.getChildren.add(imageName)
    root.getChildren.add(userName)

    params.setShowRoot(false)
    params.setRoot(root)
  }

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
  }

  def createContainer():Unit = {
    dockerClient.foreach { dc =>
      dc.containerCreate(request, name, platform) match {
        case Left(err) =>
        case Right(resp) => println(resp)
      }
    }
  }
}
