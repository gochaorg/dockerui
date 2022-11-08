package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.fxml.FXML
import javafx.scene.control.cell.TreeItemPropertyValueFactory
import javafx.scene.control.{TextField, TreeTableColumn, TreeTableView}
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.model.Image

class ImageController {
  @FXML private var inspectTreeTable:TreeTableView[Prop] = null
  @FXML private var tag_repo:TextField = null
  @FXML private var tag_tag:TextField = null

  @FXML
  def initialize():Unit = {
    val nameCol = new TreeTableColumn[Prop,String]("name")
    nameCol.setCellValueFactory(new TreeItemPropertyValueFactory[Prop,String]("name"))

    val valueCol = new TreeTableColumn[Prop,String]("value")
    valueCol.setCellValueFactory(new TreeItemPropertyValueFactory[Prop,String]("value"))

    inspectTreeTable.getColumns.clear()
    inspectTreeTable.getColumns.add(nameCol)
    inspectTreeTable.getColumns.add(valueCol)
  }

  private var currentImage:Option[Image] = None
  def onSelect(image:Image):Unit = {
    currentImage = Some(image)
    DockerClientPool.submit { dc =>
      dc.imageInspect(image.Id) match {
        case Left(err) => println(err)
        case Right(inspect) => Platform.runLater(()=>{
          Prop(inspect).foreach { root =>
            inspectTreeTable.setRoot(root)
            inspectTreeTable.setShowRoot(true)
            root.setExpanded(true)
          }
        })
      }
    }
  }

  def assignTag():Unit = {
    currentImage.foreach { ci =>
      DockerClientPool.submit { dc =>
        dc.imageTag(ci.Id,
          if (tag_repo.getText.trim.nonEmpty) {
            Some(tag_repo.getText.trim)
          } else {
            None
          },
          if (tag_tag.getText.trim.nonEmpty) {
            Some(tag_tag.getText.trim)
          } else {
            None
          }
        )
      }
    }
  }
}
