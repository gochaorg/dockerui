package xyz.cofe.lima.ui

import javafx.application.{Application, Platform}
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{Button, TextInputDialog}
import javafx.scene.layout.StackPane
import javafx.stage.Stage
import xyz.cofe.lima.docker.DockerClient

import java.util.{Timer, TimerTask}

class Main extends Application {
  override def start(primaryStage: Stage): Unit = {
    val unixSocketPath = getParameters.getNamed.get("unixSocket") match {
      case null =>
        val input = new TextInputDialog("enter path to docker.sock")
        input.setTitle("--unixSocket=??? arg is absent")
        input.setHeaderText("Enter path to unix socket file for docker")
        input.setContentText("path")
        val path = input.showAndWait()
        if( path.isEmpty ){
          None
        }else{
          Some(path.get())
        }
      case arg =>
        Some(arg)
    }

    unixSocketPath match {
      case Some(str) =>
        val loader = new FXMLLoader()

        loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/main.fxml"))
        val prnt = loader.load[Parent]()
        val controller = loader.getController[MainController]

        controller.setDockerClient(
          DockerClient.unixSocket(str)
        )

        val scene1 = new Scene(prnt)
        primaryStage.setScene(scene1)
        primaryStage.setTitle("docker ui")
        primaryStage.setHeight(600)
        primaryStage.setWidth(800)
        primaryStage.show()

        val timer = new Timer()
        timer.schedule(new TimerTask {
          override def run(): Unit = {
            Platform.runLater(()=>{
              controller.refreshByTimer()
            })
          }
        },1000, 1000)

        primaryStage.setOnCloseRequest { _ => timer.cancel() }
      case None =>
    }
  }
}

object Main {

}
