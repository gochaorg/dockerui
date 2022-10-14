package xyz.cofe.lima.ui

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.CheckBox
import javafx.stage.Stage

class RemoveContainerController {
  @FXML private var force:CheckBox = null
  @FXML private var removeAnonVolumes:CheckBox = null
  @FXML private var link:CheckBox = null
  private var okClicked=false

  private var stage:Option[Stage] = None
  def setStage(newStage: Stage):Unit = {
    stage = Some(newStage)
  }

  def isForce:Boolean = force.isSelected
  def isRemoveAnonVolumes:Boolean = removeAnonVolumes.isSelected
  def isLink:Boolean = link.isSelected
  def isOk:Boolean = okClicked

  def ok():Unit = {
    stage.foreach { s =>
      okClicked = true
      s.close()
    }
  }
  def cancel():Unit = {
    stage.foreach { s =>
      okClicked = false
      s.close()
    }
  }
}

object RemoveContainerController {
  case class Input(force:Boolean, removeAnonVolumes:Boolean, link:Boolean)

  def show():Option[Input] = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/remove-container.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[RemoveContainerController]

    val stage = new Stage()
    controller.setStage(stage)

    stage.setTitle("Remove container")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()

    if( controller.isOk ){
      Some(Input(controller.isForce,controller.isRemoveAnonVolumes,controller.isLink))
    }else{
      None
    }
  }
}
