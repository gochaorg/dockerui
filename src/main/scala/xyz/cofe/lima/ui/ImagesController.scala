package xyz.cofe.lima.ui

import javafx.beans.property.SimpleStringProperty
import javafx.fxml.FXML
import javafx.scene.control.{TableColumn, TableView}
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.model.{ContainerStatus, Image}

class ImagesController {
  @FXML private var table:TableView[Image] = null
  @FXML private var imageController:ImageController = null

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
    if( imageController!=null )imageController.setDockerClient(dc)
  }

  @FXML def initialize():Unit = {
    table.getColumns.clear()

    val repoTagsCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("RepoTags")
      tc.setCellValueFactory { param => new SimpleStringProperty(
        param.getValue.RepoTags.map(_.mkString(",")).getOrElse(""))}
      tc
    }
    table.getColumns.add(repoTagsCol)

    val idCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("Id")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.Id)}
      tc
    }
    table.getColumns.add(idCol)

    val parentIdCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("ParentId")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.ParentId)}
      tc
    }
    table.getColumns.add(parentIdCol)

    val repoDigestsCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("RepoDigests")
      tc.setCellValueFactory { param => new SimpleStringProperty(
        param.getValue.RepoDigests.map(_.mkString(",")).getOrElse("")
      )}
      tc
    }
    table.getColumns.add(repoDigestsCol)

    val createdCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("Created")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.Created.toString)}
      tc
    }
    table.getColumns.add(createdCol)

    val sizeCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("Size")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.Size.toString)}
      tc
    }
    table.getColumns.add(sizeCol)

    val sharedSizeCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("SharedSize")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.SharedSize.toString)}
      tc
    }
    table.getColumns.add(sharedSizeCol)

    val virtualSizeCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("VirtualSize")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.VirtualSize.toString)}
      tc
    }
    table.getColumns.add(virtualSizeCol)

    val containersCol : TableColumn[Image,String] = {
      val tc = new TableColumn[Image,String]("Containers")
      tc.setCellValueFactory { param => new SimpleStringProperty(param.getValue.Containers.toString)}
      tc
    }
    table.getColumns.add(containersCol)
  }

  def refresh():Unit = {
    table.getItems.clear()
    dockerClient.foreach { dc =>
      dc.images() match {
        case Left(err) => println(err)
        case Right(images) =>
          images.foreach(table.getItems.add)
      }
    }
  }
  def pullImage():Unit = {
    dockerClient.foreach { dc =>
      PullImageController.show(dc)
    }
  }
  def deleteSelected():Unit={
    DeleteImageController.show() match {
      case None =>
      case Some(params) =>
        dockerClient.foreach { dc =>
          table.getSelectionModel.getSelectedItems.forEach(img => {
            dc.imageRemove(img.Id, Some(params.force), Some(params.noprune)) match {
              case Left(err) => println(err)
              case Right(_) =>
            }
          })
        }
    }
  }
}
