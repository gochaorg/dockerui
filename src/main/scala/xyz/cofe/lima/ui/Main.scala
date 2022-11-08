package xyz.cofe.lima.ui

import javafx.application.{Application, Platform}
import javafx.fxml.FXMLLoader
import javafx.scene.control.TextInputDialog
import javafx.scene.{Parent, Scene}
import javafx.stage.Stage
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.http.HttpLogger
import xyz.cofe.lima.docker.log.Logger
import xyz.cofe.lima.store.AppHome
import xyz.cofe.lima.store.log.{AppendableFile, FilesCleaner, PathPattern}

import java.nio.file.Path
import java.util.{Timer, TimerTask}

class Main extends Application {
  override def start(primaryStage: Stage): Unit = {
    val unixSocket_arg = getParameters.getNamed.get("unixSocket") match {
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

    getParameters.getNamed.get("appHome") match {
      case null => ()
      case arg => AppHome.setSystemParam(Path.of(arg))
    }

    unixSocket_arg match {
      case Some(str) =>
        val loader = new FXMLLoader()

        loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/main.fxml"))
        val prnt = loader.load[Parent]()
        val controller = loader.getController[MainController]

        implicit val httpLogger: HttpLogger =
          HttpLogger.JsonLogger(
            AppendableFile(
              PathPattern.escape(AppHome.directory) ++
              PathPattern.parse(Path.of("log/http/{yyyy}-{MM}/{dd}/{hh}-{mm}-{ss}.stream.json")),
              limitSizePerFile = Some(1024L*512L)
            )
          )

        val logToFile = Logger.JsonToWriter(
          AppendableFile(
            PathPattern.escape(AppHome.directory) ++
              PathPattern.parse(Path.of("log/dockerClient/{yyyy}-{MM}/{dd}/{hh}-{mm}-{ss}.stream.json")),
            limitSizePerFile = Some(1024L * 512L)
          )
        )

        val failLogStdout = Logger.failLogger(
          Logger.JsonToWriter(System.out)
        )

        val dc = DockerClient.unixSocket(str)
          .withLogger(
            Logger.joinLoggers(logToFile, failLogStdout)
          )

        DockerClientPool.init(new DockerClientPool(dc))

        val logCleaner = new Thread(()=>{
          FilesCleaner.clean(AppHome.directory.resolve("log/dockerClient"), 1024L * 1024L * 32L)
          FilesCleaner.clean(AppHome.directory.resolve("log/http"), 1024L * 1024L * 32L)
        })
        logCleaner.setDaemon(true)
        logCleaner.start()

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
      case None =>
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

