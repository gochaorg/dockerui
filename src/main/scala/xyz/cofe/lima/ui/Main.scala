package xyz.cofe.lima.ui

import javafx.application.{Application, Platform}
import javafx.fxml.FXMLLoader
import javafx.scene.control.TextInputDialog
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.http.HttpLogger
import xyz.cofe.lima.docker.hub.DockerHubClient
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.store.AppHome
import xyz.cofe.lima.store.config.{AppConfig, DockerConnect}
import xyz.cofe.lima.store.log.{AppendableFile, FilesCleaner, PathPattern}

import java.nio.file.Path
import java.util.{Timer, TimerTask}

class Main extends Application {
  override def start(primaryStage: Stage): Unit = {
    getParameters.getNamed.get("appHome") match {
      case null => ()
      case arg => AppHome.setSystemParam(Path.of(arg))
    }

    val configEt = (AppHome.readConfig match {
      case Left(readConfigErr) =>
        println(readConfigErr)
        getParameters.getNamed.get("unixSocket") match {
          case null =>
            val input = new TextInputDialog("enter path to docker.sock")
            input.setTitle("--unixSocket=??? arg is absent")
            input.setHeaderText("Enter path to unix socket file for docker")
            input.setContentText("path")
            val path = input.showAndWait()
            if (path.isEmpty) {
              Left(s"docker.sock not defined, readConfigErr: $readConfigErr")
            } else {
              Right(AppConfig.defaultConfig(Path.of(path.get())))
            }
          case arg => Right(
            AppConfig.defaultConfig(Path.of(arg))
          )
        }
      case Right(value) =>
        Right(value)
    }).map( config =>
      getParameters.getNamed.get("unixSocket") match {
        case null => config
        case arg =>
          config.copy(
            dockerConnect = {
              config.dockerConnect match {
                case u:DockerConnect.UnixSocketFile =>
                  u.copy(fileName = arg)
              }
            }
          )
      }
    )

    configEt match {
      case Right(config) =>
        val loader = new FXMLLoader()

        loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/main.fxml"))
        val prnt = loader.load[Parent]()
        val controller = loader.getController[MainController]
        controller.setAppConfig(config)

        DockerClientPool.init(new DockerClientPool(config.dockerConnect.createDockerClient()))
        DockerHubClientPool.init(new DockerHubClientPool(DockerHubClient(java.net.http.HttpClient.newBuilder().build())))

        val scene1 = new Scene(prnt)
        primaryStage.setScene(scene1)
        primaryStage.setTitle("docker ui")
        primaryStage.setHeight(600)
        primaryStage.setWidth(800)
        primaryStage.show()

        controller.changeTimerPeriodCallback { period =>
          runTimer(period, ()=>{
            controller.refreshByTimer()
          })
          controller.onTimerPeriod(period)
        }

        val initialTimerPeriodSeconds = 3
        runTimer(initialTimerPeriodSeconds,()=>{
          controller.refreshByTimer()
        })
        controller.onTimerPeriod(initialTimerPeriodSeconds)

        primaryStage.setOnCloseRequest { _ => timer.cancel() }
      case Left(err) =>
        println(err)
        System.exit(1)
    }
  }

  private lazy val timer = new Timer()
  private var timerTasks = List[TimerTask]()

  def runTimer(periodInSeconds:Int, runner:()=>Unit):Unit = {
    timerTasks.foreach { tt => tt.cancel() }
    timerTasks = List()
    if( periodInSeconds>0 ){
      val tt = new TimerTask {
        override def run(): Unit = {
          Platform.runLater(()=>{
            runner()
          })
        }
      }
      timerTasks = tt :: timerTasks
      timer.schedule(tt,500, periodInSeconds*1000)
    }
  }
}

