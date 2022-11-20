package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.property.{SimpleStringProperty, StringProperty}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, Label, TableColumn, TextField, TreeItem, TreeTableColumn, TreeTableView}
import javafx.scene.input.KeyCode
import javafx.stage.Stage
import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.hub.{model => hmodel}

import java.util.concurrent.atomic.AtomicInteger

/** Поиск образов на docker hub */
class SearchImageController {
  import SearchImageController._
  @FXML private var searchTree : TreeTableView[SearchNode] = null
  @FXML private var searchQuery : TextField = null
  @FXML private var downloadButton : Button = null
  @FXML private var message : Label = null

  private val root = new TreeItem[SearchNode](new SearchRoot())
  @FXML def initialize():Unit = {
    val nameColumn : TreeTableColumn[SearchNode,String] = {
      val tc = new TreeTableColumn[SearchNode,String]("name")
      tc.setCellValueFactory { param => new SimpleStringProperty(
        param.getValue.getValue match {
          case SearchRoot() => "root"
          case SearchImageResp(img) => img.name.getOrElse("?")
          case SearchTag(img) => img.name.getOrElse("?")
          case SearchImageTag(img) => List(img.os.getOrElse("?"), img.architecture.getOrElse("?"), img.status.getOrElse("?")).mkString(" ")
        }
      )}
      tc
    }

    val commentColumn: TreeTableColumn[SearchNode, String] = {
      val tc = new TreeTableColumn[SearchNode, String]("comment")
      tc.setCellValueFactory { param =>
        new SimpleStringProperty(
          param.getValue.getValue match {
            case SearchRoot() => "root"
            case SearchImageResp(img) => "found on hub"
            case SearchTag(img) => "tag"
            case SearchImageTag(img) => "image"
          }
        )
      }
      tc
    }

    searchTree.getColumns.clear()
    searchTree.getColumns.add(nameColumn)
    searchTree.getColumns.add(commentColumn)

    searchTree.setShowRoot(false)
    searchTree.setRoot(root)

    searchQuery.setOnKeyReleased { ev =>
      if( ev.getCode == KeyCode.ENTER ){
        find()
      }
    }
    message.setOnMouseClicked(ev => {updateMessage()})
  }

  private val runningRequests = new AtomicInteger(0)

  private def updateMessage():Unit = {
    def doUpdate():Unit = {
      message.setText(s"running ${runningRequests} request")
    }

    if( Platform.isFxApplicationThread ){
      doUpdate()
    }else{
      Platform.runLater(()=>doUpdate())
    }
  }

  private def changeRunnRequestCount(code: AtomicInteger => Unit):Unit = {
    code(runningRequests)
    updateMessage()
  }

  private def incRequestCount():Unit = changeRunnRequestCount(cnt=>{cnt.incrementAndGet()})
  private def decRequestCount():Unit = changeRunnRequestCount(cnt=>{cnt.decrementAndGet()})

  def find():Unit = {
    val term = searchQuery.getText

    incRequestCount()
    DockerClientPool.submit { dc =>
      try {
        val res = dc.imageSearch(term, None)
        if (res.isLeft) println(res)

        res.foreach(listOfImageSearch => {
          Platform.runLater(() => {
            foundImageSearch(listOfImageSearch)
          })
        })
      } finally {
        decRequestCount()
      }
    }
  }

  private def foundImageSearch(imageSearches:List[model.ImageSearch]):Unit = {
    root.getChildren.clear()
    imageSearches.foreach { img =>
      val node = new TreeItem[SearchNode](SearchImageResp(img))
      root.getChildren.add(node)

      hmodel.TagsRequest(img) match {
        case Left(err) => println(err)
        case Right(req) =>
          incRequestCount()
          DockerHubClientPool.submit { dhc =>
            try {
              val tagsEt = dhc.tags(req)
              if (tagsEt.isLeft) println(tagsEt)

              Platform.runLater(() => {
                tagsEt.foreach { tags =>
                  tags.results.foreach { tag =>
                    val tagNode = new TreeItem[SearchNode](SearchTag(tag))
                    tag.images.foreach { img =>
                      val imgNode = new TreeItem[SearchNode](SearchImageTag(img))
                      tagNode.getChildren.add(imgNode)
                    }
                    node.getChildren.add(tagNode)
                    node.setExpanded(true)
                  }
                }
              })
            } finally {
              decRequestCount()
            }
          }
      }
    }
  }

  def download():Unit = {}
}

object SearchImageController {
  sealed trait SearchNode
  case class SearchRoot() extends SearchNode
  case class SearchImageResp( searchImage:model.ImageSearch ) extends SearchNode
  case class SearchTag( tag:hmodel.Tag ) extends SearchNode
  case class SearchImageTag(img:hmodel.ImageTag) extends SearchNode

  def show():Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/search-image.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[SearchImageController]

    val stage = new Stage()
    //stage.setOnCloseRequest { _ => controller.closing() }

    stage.setTitle("Search on docker hub")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()
  }
}