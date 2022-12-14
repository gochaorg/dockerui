package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.fxml.FXML
import javafx.scene.control
import javafx.scene.control.cell.TreeItemPropertyValueFactory
import javafx.scene.control.{TextArea, TextField, TreeTableColumn, TreeTableView}
import xyz.cofe.lima.docker.{DockerClient, model}

class ContainerController {
  @FXML
  private var treeTable : TreeTableView[Prop] = null

  @FXML
  private var logsStdOut: TextArea = null

  @FXML
  private var logsStdErr: TextArea = null

  @FXML
  def initialize():Unit = {
    val nameCol = new TreeTableColumn[Prop,String]("name")
    nameCol.setCellValueFactory(new TreeItemPropertyValueFactory[Prop,String]("name"))

    val valueCol = new TreeTableColumn[Prop,String]("value")
    valueCol.setCellValueFactory(new TreeItemPropertyValueFactory[Prop,String]("value"))

    treeTable.getColumns.clear()
    treeTable.getColumns.add(nameCol)
    treeTable.getColumns.add(valueCol)
  }

  private var containerId:Option[String] = None

  def select(cId:Option[String]):Unit = {
    containerId = cId
    containerId.foreach(cid => {
      DockerClientPool.submit( dc => {
        dc.containerInspect(cid) match {
          case Left(err) =>
          case Right(ci) => Platform.runLater(()=>{
            Prop(ci).foreach(root => {
              root.setExpanded(true)
              treeTable.setRoot(root)
            })
          })
        }
      })

      DockerClientPool.submit( dc => {
        dc.containerLogs(cid, stdout = Some(true)) match {
          case Left(err) =>
          case Right(logs) =>
            Platform.runLater(()=> {
              logsStdOut.setText(logs.mkString("\n"))
            })
        }
      })

      DockerClientPool.submit( dc => {
        dc.containerLogs(cid, stderr = Some(true)) match {
          case Left(err) =>
          case Right(logs) => Platform.runLater(()=> {
            logsStdErr.setText(logs.mkString("\n"))
          })
        }
      })
    })
  }

  def refreshInspect():Unit = {
    containerId.foreach { cid =>
      DockerClientPool.submit { dc =>
        dc.containerInspect(cid) match {
          case Left(err) =>
          case Right(ci) => Platform.runLater(()=>{
            Prop(ci).foreach(root => {
              root.setExpanded(true)
              treeTable.setRoot(root)
            })
          })
        }
      }
    }
  }

  def refreshLogsStdOut():Unit = {
    containerId.foreach(cid => {
      DockerClientPool.submit { dc =>
        dc.containerLogs(cid, stdout = Some(true)) match {
          case Left(err) =>
          case Right(logs) => Platform.runLater(()=> {
            logsStdOut.setText(logs.mkString("\n"))
          })
        }
      }
    })
  }

  def refreshLogsStdErr():Unit = {
    containerId.foreach(cid => {
      DockerClientPool.submit { dc =>
        dc.containerLogs(cid, stderr = Some(true)) match {
          case Left(err) =>
          case Right(logs) => Platform.runLater(()=> {
            logsStdErr.setText(logs.mkString("\n"))
          })
        }
      }
    })
  }

  @FXML
  private var newName : TextField = null

  def rename():Unit = {
    if( newName!=null && newName.getText.trim.nonEmpty ) {
      containerId.foreach { cId =>
        DockerClientPool.submit { dc =>
          dc.containerRename(cId, newName.getText.trim)
        }
      }
    }
  }

  def delete():Unit = {
    containerId.foreach { cId =>
      RemoveContainerController.show() match {
        case Some(deleteParams) =>
          DockerClientPool.submit { dc =>
            dc.containerRemove(cId, Some(deleteParams.removeAnonVolumes), Some(deleteParams.force), Some(deleteParams.link))
          }
        case None =>
      }
    }
  }
}
