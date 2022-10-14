package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control
import javafx.scene.control.cell.TreeItemPropertyValueFactory
import javafx.scene.control.{TextArea, TextField, TreeTableColumn, TreeTableView}
import xyz.cofe.lima.docker.{DockerClient, model}
import xyz.cofe.lima.{TreeShowDerivation, TreeWriter}

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

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
  }

  private var containerId:Option[String] = None

  def select(cId:Option[String]):Unit = {
    containerId = cId
    containerId.foreach(cid => {
      dockerClient.foreach(dc => {
        dc.containerInspect(cid) match {
          case Left(err) =>
            println(err)
          case Right(ci) =>
            Prop(ci).foreach(root => {
              root.setExpanded(true)
              treeTable.setRoot(root)
            })
        }

        dc.containerLogs(cid, stdout = Some(true)) match {
          case Left(err) => println(err)
          case Right(logs) =>
            logsStdOut.setText(logs.mkString("\n"))
        }

        dc.containerLogs(cid, stderr = Some(true)) match {
          case Left(err) => println(err)
          case Right(logs) =>
            logsStdErr.setText(logs.mkString("\n"))
        }
      })
    })
  }

  def refreshLogsStdOut():Unit = {
    containerId.foreach(cid => {
      dockerClient.foreach(dc => {
        dc.containerLogs(cid, stdout = Some(true)) match {
          case Left(err) => println(err)
          case Right(logs) =>
            logsStdOut.setText(logs.mkString("\n"))
        }
      })
    })
  }

  def refreshLogsStdErr():Unit = {
    containerId.foreach(cid => {
      dockerClient.foreach(dc => {
        dc.containerLogs(cid, stderr = Some(true)) match {
          case Left(err) => println(err)
          case Right(logs) =>
            logsStdErr.setText(logs.mkString("\n"))
        }
      })
    })
  }

  @FXML
  private var newName : TextField = null

  def rename():Unit = {
    if( newName!=null && newName.getText.trim.nonEmpty ) {
      dockerClient.foreach { dc =>
        containerId.foreach { cId =>
          dc.containerRename(cId, newName.getText.trim)
        }
      }
    }
  }

  def delete():Unit = {
    dockerClient.foreach { dc =>
      containerId.foreach { cId =>
        RemoveContainerController.show() match {
          case Some(deleteParams) =>
            dc.containerRemove(cId, Some(deleteParams.removeAnonVolumes), Some(deleteParams.force), Some(deleteParams.link)) match {
              case Left(err) =>
                println(err)
              case Right(_) => ()
            }
          case None =>
        }
      }
    }
  }
}
