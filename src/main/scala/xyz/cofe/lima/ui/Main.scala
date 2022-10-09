package xyz.cofe.lima.ui

import javafx.application.{Application, Platform}
import javafx.fxml.FXMLLoader
import javafx.scene.{Parent, Scene}
import javafx.scene.control.Button
import javafx.scene.layout.StackPane
import javafx.stage.Stage
import xyz.cofe.lima.docker.DockerClient

import java.util.{Timer, TimerTask}

class Main extends Application {
  override def start(primaryStage: Stage): Unit = {
//    val but = new Button("Hello")
//    but.setOnAction({ ev =>
//      println("clicked")
//    })
//
//    val stack = new StackPane()
//    stack.getChildren.add(but)
//
//    val scene = new Scene(stack, 600, 400)
//    primaryStage.setScene(scene)
//    primaryStage.setTitle("lima ui")
//    primaryStage.show()

    val loader = new FXMLLoader()

    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/containers.fxml"))
    val prnt = loader.load[Parent]()
    val controller = loader.getController[ContainersController]

    controller.setDockerClient(
      DockerClient.unixSocket("/Users/g.kamnev/.colima/docker.sock")
    )

    val scene1 = new Scene(prnt)
    primaryStage.setScene(scene1)
    primaryStage.setTitle("lima ui")
    primaryStage.show()

    val timer = new Timer()
    timer.schedule(new TimerTask {
      override def run(): Unit = {
        Platform.runLater(()=>{
          controller.refreshByTimer()
        })
      }
    },1000, 3000
    )

    primaryStage.setOnCloseRequest { ev =>
      timer.cancel()
    }
  }
}

object Main {

}
