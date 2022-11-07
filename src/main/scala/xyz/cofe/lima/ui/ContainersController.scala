package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.beans.{InvalidationListener, Observable}
import javafx.beans.property.SimpleStringProperty
import javafx.collections.ListChangeListener
import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{SelectionMode, TableColumn, TableView}
import javafx.stage.Stage
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.model.ContainerStatus

import scala.collection.JavaConverters._

class ContainersController {
  @FXML
  private var table : TableView[ContainerStatus] = null

  private lazy val idCol : TableColumn[ContainerStatus,String] = {
    val tc = new TableColumn[ContainerStatus,String]("id")
    tc.setCellValueFactory { param => new SimpleStringProperty( param.getValue.Id ) }
    tc
  }
  private lazy val nameCol : TableColumn[ContainerStatus,String] = {
    val tc = new TableColumn[ContainerStatus,String]("name")
    tc.setCellValueFactory { param => new SimpleStringProperty(
      param.getValue.Names.mkString(",")
    )}
    tc
  }
  private lazy val imageCol : TableColumn[ContainerStatus,String] = {
    val tc = new TableColumn[ContainerStatus,String]("image")
    tc.setCellValueFactory { param => new SimpleStringProperty(
      param.getValue.Image
    )}
    tc
  }
  private lazy val stateCol : TableColumn[ContainerStatus,String] = {
    val tc = new TableColumn[ContainerStatus,String]("state")
    tc.setCellValueFactory { param => new SimpleStringProperty(
      param.getValue.State
    )}
    tc
  }
  private lazy val statusCol : TableColumn[ContainerStatus,String] = {
    val tc = new TableColumn[ContainerStatus,String]("status")
    tc.setCellValueFactory { param => new SimpleStringProperty(
      param.getValue.Status
    )}
    tc
  }
  private lazy val commandCol : TableColumn[ContainerStatus,String] = {
    val tc = new TableColumn[ContainerStatus,String]("command")
    tc.setCellValueFactory { param => new SimpleStringProperty(
      param.getValue.Command
    )}
    tc
  }

  @FXML
  private var containerController: ContainerController = null

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
    if( containerController!=null ) containerController.setDockerClient(dc)
  }

  @FXML
  def initialize():Unit = {
    if( table!=null ){
      table.getColumns.clear()
      table.getColumns.add(idCol)
      table.getColumns.add(nameCol)
      table.getColumns.add(imageCol)
      table.getColumns.add(stateCol)
      table.getColumns.add(statusCol)
      table.getColumns.add(commandCol)

      table.getSelectionModel.setSelectionMode(SelectionMode.MULTIPLE)
    }

    if( containerController!=null ){
      if( table!=null ){
        table.getSelectionModel.getSelectedItems.addListener(new ListChangeListener[ContainerStatus] {
          override def onChanged(c: ListChangeListener.Change[_ <: ContainerStatus]): Unit = {
            if (!refreshByTimerRunning) {
              while (c.next()) {
                if (c.wasPermutated()
                ||  c.wasAdded()
                ||  c.wasUpdated()
                ||  c.wasRemoved()
                ||  c.wasReplaced()
                ) {
                  c.getList.asScala.headOption.foreach(ci=>onSelectContainer(ci.Id))
                }
              }
            }
          }
        })
      }
    }
  }

  private def onSelectContainer(id:String):Unit = {
    if( containerController!=null ){
      containerController.select(Some(id))
    }
  }

  def refresh():Unit = {
    if( table!=null ){
      table.getItems.clear()
      DockerClientPool.submit { dc =>
        val res = dc.containers(all = true)
        Platform.runLater(()=>{
          res.foreach { _.foreach { c => table.getItems.add(c) } }
        })
      }
    }
  }

  private var refreshByTimerRunning = false

  def refreshByTimer():Unit = {
    if( table!=null ){
      DockerClientPool.submit { dc =>
        dc.containers(all = true) match {
          case Left(err) => println(err)
          case Right(newContainers) => {
            Platform.runLater(()=>{
              try {
                refreshByTimerRunning = true
                val selected = selectedContainersId()
                sync(newContainers)
                setSelectedContainersId(selected)
              } finally {
                refreshByTimerRunning = false
              }
            })
          }
        }
      }
    }
  }

  private def selectedContainersId():Seq[String] = {
    if( table==null ){
      List()
    }else{
      table.getSelectionModel.getSelectedItems.asScala.map(_.Id).toSeq
    }
  }
  private def containersFromTable(ids:Seq[String]) = {
    if( table==null ){
      List()
    }else{
      val idSet = ids.toSeq
      table.getItems.asScala.filter { c => idSet.contains(c.Id) }
    }
  }
  private def setSelectedContainersId(ids:Seq[String]):Unit = {
    if( table!=null ){
      table.getSelectionModel.clearSelection()
      containersFromTable(ids).foreach { c =>
        table.getSelectionModel.select(c)
      }
    }
  }

  private def sync(actualContainers:List[ContainerStatus]):Unit = {
    import scala.collection.JavaConverters._
    val oldContainers = table.getItems.asScala.map { c => (c.Id -> c) }.toMap
    val addList = actualContainers.filter { ctr => !oldContainers.contains(ctr.Id) }

    val newContainers1 = actualContainers.map( c => (c.Id -> c) ).toMap
    val removeList = oldContainers.filter { case (id, c) => !newContainers1.contains(id) }.values
    removeList.foreach { c => table.getItems.remove(c) }
    addList.foreach { c => table.getItems.add(c) }

    table.getItems.asScala.zipWithIndex.map { case(ec,idx) =>
      (idx, ec, newContainers1.get(ec.Id))
    }.filter { case (i,oc,nc) =>
      nc.isDefined
    }.map { case (i,oc,nc) =>
      (i,oc,nc.get)
    }.foreach { case (i,oc,nc) =>
      if( oc!=nc ){
        table.getItems.set(i,nc)
      }
    }
  }

  def startSelected():Unit = {
    if( table!=null ){
      table.getSelectionModel.getSelectedItems.forEach(c => {
        DockerClientPool.submit { dc =>
          dc.containerStart(c.Id)
        }
      })
    }
  }
  def stopSelected():Unit = {
    if( table!=null ){
      table.getSelectionModel.getSelectedItems.forEach(c => {
        DockerClientPool.submit { dc =>
          dc.containerStop(c.Id)
        }
      })
    }
  }
  def createContainer():Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/create-container.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[CreateContainerController]

    val stage = new Stage()
    stage.setTitle("Create container")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()
    controller.prepareEdit()
    dockerClient.foreach(dc => controller.setDockerClient(dc))
  }

  def deleteSelected():Unit = {
    RemoveContainerController.show() match {
      case None =>
      case Some(deleteParams) =>
        if( table!=null ){
            table.getSelectionModel.getSelectedItems.forEach(c => {
              DockerClientPool.submit { dc =>
                dc.containerRemove(c.Id, Some(deleteParams.removeAnonVolumes), Some(deleteParams.force), Some(deleteParams.link)) match {
                  case Left(err) =>
                  case Right(value) => ()
                }
              }
            })
          }
        }
  }
}
