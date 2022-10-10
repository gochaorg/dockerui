package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control.cell.TreeItemPropertyValueFactory
import javafx.scene.control.{TextArea, TreeTableColumn, TreeTableView}
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
        dc.inspectContainer(cid) match {
          case Left(err) =>
            println(err)
          case Right(ci) =>
            Prop(ci).foreach(root => {
              root.setExpanded(true)
              treeTable.setRoot(root)
            })
        }

        dc.logs(cid, stdout = Some(true)) match {
          case Left(err) => println(err)
          case Right(logs) =>
            logsStdOut.setText(logs.mkString("\n"))
        }

        dc.logs(cid, stderr = Some(true)) match {
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
        dc.logs(cid, stdout = Some(true)) match {
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
        dc.logs(cid, stderr = Some(true)) match {
          case Left(err) => println(err)
          case Right(logs) =>
            logsStdErr.setText(logs.mkString("\n"))
        }
      })
    })
  }
}
