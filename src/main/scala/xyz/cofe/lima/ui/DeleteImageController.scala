package xyz.cofe.lima.ui

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{CheckBox, Label}
import javafx.stage.Stage
import xyz.cofe.lima.docker.DockerClient

class DeleteImageController {
  @FXML private var message : Label = null
  @FXML private var force : CheckBox = null
  @FXML private var noprune : CheckBox = null
  private var okClicked = false
  private var stage:Option[Stage]=None
  def ok():Unit = {
    okClicked = true
    stage.foreach { s => s.close() }
  }
  def cancel():Unit = {
    okClicked = false
    stage.foreach { s => s.close() }
  }
  def message(msg:String):Unit = {
    message.setText(msg)
    message.setVisible(msg.nonEmpty)
  }
  def setStage(newStage:Stage):Unit = { stage=Some(newStage) }
  def isOk = okClicked
  def isForce = force.isSelected
  def isNoprune = noprune.isSelected
}

object DeleteImageController {
  case class DeleteParams(force:Boolean, noprune:Boolean)
  def show(message:String=""):Option[DeleteParams] = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/delete-image.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[DeleteImageController]

    val stage = new Stage()
    controller.setStage(stage)
    controller.message(message)

    stage.setTitle("Delete image")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.showAndWait()

    if( controller.isOk ){
      Some(DeleteParams(force = controller.isForce, noprune = controller.isNoprune))
    }else{
      None
    }
  }
}
