package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.property.SimpleStringProperty
import javafx.fxml.FXML
import javafx.scene.control.{SelectionMode, TableColumn, TableView}
import xyz.cofe.lima.docker.model.Image

import scala.collection.JavaConverters._

class ImagesController {
  @FXML private var table:TableView[Image] = null
  @FXML private var imageController:ImageController = null

  @FXML def initialize():Unit = {
    table.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
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

  private lazy val syncTable = SyncTable[Image,String](table, im=>im.Id, (a,b)=>a==b)
    .trackFocused(imageController.onSelect)

  def refresh():Unit = {
    DockerClientPool.submit { dc =>
      dc.images() match {
        case Left(err) =>
        case Right(images) => Platform.runLater(()=>{
          syncTable.sync(images)
        })
      }
    }
  }
  def refreshByTimer():Unit = {
    refresh()
  }

  def pullImage():Unit = {
    PullImageController.show
  }

  def deleteSelected():Unit={
    val tags = table.getSelectionModel.getSelectedItems.asScala.map(img => {
      img.RepoTags.map(_.mkString(",")).getOrElse("")
    }).filter(_.nonEmpty).toList

    val msg = if (tags.nonEmpty) {
      s"Delete images:\n" + tags.mkString("\n")
    } else {
      ""
    }

    DeleteImageController.show(msg) match {
      case None =>
      case Some(params) =>
        val images = table.getSelectionModel.getSelectedItems.asScala.toList
        DockerClientPool.submit { dc =>
          images.foreach(img => {
            dc.imageRemove(img.Id, Some(params.force), Some(params.noprune))
          })
        }
    }
  }
}
