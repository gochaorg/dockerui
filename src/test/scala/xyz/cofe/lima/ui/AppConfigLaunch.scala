package xyz.cofe.lima.ui

import javafx.application.Application
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage

class AppConfigLaunch extends Application {
  override def start(primaryStage: Stage): Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/test/app-config.fxml"))

    val parent = loader.load[Parent]()
    //val controller = loader.getController[AppConfigController1234]

    primaryStage.setTitle("App config")
    primaryStage.setWidth(400)
    primaryStage.setHeight(600)

    val scene = new Scene(parent)
    primaryStage.setScene(scene)
    primaryStage.show()
  }
}

object AppConfigLaunch {
  def main(args:Array[String]):Unit = {
    Application.launch(classOf[AppConfigLaunch],args:_*)
  }
}
