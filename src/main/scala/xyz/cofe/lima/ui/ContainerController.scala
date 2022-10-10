package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control.cell.TreeItemPropertyValueFactory
import javafx.scene.control.{TreeItem, TreeTableColumn, TreeTableView}
import xyz.cofe.lima.docker.DockerClient

class ContainerController {
  import ContainerController._

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

//    val prop0 = new TreeItem[Prop](new Prop("prop0","val0"))
//    val prop1 = new TreeItem[Prop](new Prop("prop1","val1"))
//    val prop2 = new TreeItem[Prop](new Prop("prop2","val2"))
//    prop0.getChildren.add(prop1)
//    prop0.getChildren.add(prop2)
//    treeTable.setRoot(prop0)
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
            val root = new TreeItem(Prop("root","root"))
            root.getChildren.add(new TreeItem(Prop("Id",ci.Id)))
            root.getChildren.add(new TreeItem(Prop("Name",ci.Name)))
            root.getChildren.add(new TreeItem(Prop("Image",ci.Image)))
            root.getChildren.add(new TreeItem(Prop("Path",ci.Path)))
            root.getChildren.add(new TreeItem(Prop("Driver",ci.Driver)))
            root.getChildren.add(new TreeItem(Prop("AppArmorProfile",ci.AppArmorProfile)))
            root.getChildren.add(new TreeItem(Prop("Created",ci.Created)))
            root.getChildren.add(new TreeItem(Prop("HostnamePath",ci.HostnamePath)))
            root.getChildren.add(new TreeItem(Prop("HostsPath",ci.HostsPath)))
            root.getChildren.add(new TreeItem(Prop("LogPath",ci.LogPath)))
            root.getChildren.add(new TreeItem(Prop("MountLabel",ci.MountLabel)))
            root.getChildren.add(new TreeItem(Prop("Platform",ci.Platform)))
            root.getChildren.add(new TreeItem(Prop("ProcessLabel",ci.ProcessLabel)))
            root.getChildren.add(new TreeItem(Prop("ResolvConfPath",ci.ResolvConfPath)))
            treeTable.setRoot(root)
        }
      })
    })
  }
}

object ContainerController {
  case class Prop(name:String, value:String) {
    def getName():String = name
    def setName(str:String):Unit = ()

    def getValue():String = value
    def setValue(str:String):Unit = ()
  }
}
