package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.control._
import javafx.scene.input.KeyCode
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import xyz.cofe.lima.docker.hub.{model => hmodel}
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.docker.model

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
          case SearchImageResp(img,_) => img.name.getOrElse("?")
          case SearchTag(_,img) => img.name.getOrElse("?")
          case SearchImageTag(_,img,_) => List(img.os.getOrElse("?"), img.architecture.getOrElse("?"), img.status.getOrElse("?")).mkString(" ")
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
            case SearchImageResp(img,state) => state
            case SearchTag(_,_) => "tag"
            case _:SearchImageTag => "image"
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

    val changeListener : ChangeListener[Any] = new ChangeListener[Any] {
      def changed(observable: ObservableValue[_ <: Any], oldValue: Any, newValue: Any): Unit = {
        onSelectChanged()
      }
    }
    searchTree.getSelectionModel.selectedItemProperty().addListener(changeListener)

    downloadButton.setDisable(true)
  }

  private def selection:List[SearchNode] = {
    var ls = List[SearchNode]()
    searchTree.getSelectionModel.getSelectedItems.forEach(ti => {
      ls = ti.getValue :: ls
    })
    ls
  }

  private def onSelectChanged():Unit = {
    if( selection.forall {
      case _:SearchImageTag => true
      case _:SearchTag => true
      case _ => false
    } ) {
      downloadButton.setDisable(false)
    }else{
      downloadButton.setDisable(true)
    }
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
    imageSearches.foreach { searchImg =>
      val node = new TreeItem[SearchNode](SearchImageResp(searchImg,"search tags"))
      root.getChildren.add(node)

      hmodel.TagsRequest(searchImg) match {
        case Left(err) => println(err)
        case Right(req) =>
          incRequestCount()
          DockerHubClientPool.submit { dhc =>
            try {
              val tagsEt = dhc.tags(req)

              Platform.runLater(() => {
                tagsEt.left.foreach { err =>
                  node.setValue(SearchImageResp(searchImg, s"error: $err"))
                }
                tagsEt.foreach { tags =>
                  tags.results.foreach { tag =>
                    val tagNode = new TreeItem[SearchNode](SearchTag(searchImg,tag))
                    tag.images.foreach { img =>
                      val imgNode = new TreeItem[SearchNode](SearchImageTag(searchImg,img,tag))
                      tagNode.getChildren.add(imgNode)
                    }
                    node.getChildren.add(tagNode)
                  }
                  node.setExpanded(true)
                  node.setValue(SearchImageResp(searchImg, s"found ${tags.results.length} tags, ${tags.results.flatMap(_.images).length} images"))
                }
              })
            } catch {
              case err:Throwable =>
                println(s"err ${err.toString}")
                err.printStackTrace()
            } finally {
              decRequestCount()
            }
          }
      }
    }
  }

  def download():Unit = {
    selection.foreach {
      case _:SearchRoot =>
      case _:SearchImageResp =>
      case SearchTag(searchImage,img) =>
        PullImageController.show(
          Logger.ImageCreate(
            fromImage = searchImage.name,
            tag = img.name
          )
        )
      case SearchImageTag(searchImage,img,tag) =>
        PullImageController.show(
          Logger.ImageCreate(
            fromImage=searchImage.name,
            tag=tag.digest
          )
        )
    }
  }
}

object SearchImageController {
  sealed trait SearchNode
  case class SearchRoot() extends SearchNode
  case class SearchImageResp( searchImage:model.ImageSearch, state:String="init" ) extends SearchNode
  case class SearchTag( searchImage:model.ImageSearch,tag:hmodel.Tag ) extends SearchNode
  case class SearchImageTag(
                             searchImage:model.ImageSearch,
                             img:hmodel.ImageTag,
                             tag:hmodel.Tag
                           ) extends SearchNode

  def show():Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/search-image.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[SearchImageController]

    val stage = new Stage()

    stage.setTitle("Search on docker hub")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()
  }
}