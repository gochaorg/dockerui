package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control.cell.TreeItemPropertyValueFactory
import javafx.scene.control.{TreeItem, TreeTableColumn, TreeTableView}
import xyz.cofe.lima.{TreeShowDerivation, TreeWriter}
import xyz.cofe.lima.docker.{DockerClient, model}

class ContainerController {
  @FXML
  private var treeTable : TreeTableView[Prop] = null

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

  def select(containerId:Option[String]):Unit = {
    containerId.foreach(cid => {
      dockerClient.foreach(dc => {
        dc.inspectContainer(cid) match {
          case Left(err) =>
            println(err)
          case Right(ci) =>
            val collector = TreeWriter.Writer()

            val showTree = TreeShowDerivation.gen[model.ContainerInspect]
            showTree.show(collector,ci)

            collector.current match {
              case Some(tn) =>
                treeTable.setRoot(Prop.tree(tn))
              case None =>
            }
        }
      })
    })
  }
}
