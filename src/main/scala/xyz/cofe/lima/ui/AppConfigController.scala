package xyz.cofe.lima.ui

import javafx.fxml.{FXML, FXMLLoader}
import javafx.scene.{Parent, Scene}
import javafx.scene.control.{CheckBox, TextField}
import javafx.stage.Stage
import xyz.cofe.lima.docker.http.{Duration, SocketReadTimings}
import xyz.cofe.lima.store.AppHome
import xyz.cofe.lima.store.config.{AppConfig, DockerConnect}

class AppConfigController {
  @FXML private var unixSocket:TextField = null;
  @FXML private var readTimeoutSet:CheckBox = null;
  @FXML private var readTimeoutValue:TextField = null;
  @FXML private var sourceTimeoutSet:CheckBox = null;
  @FXML private var sourceTimeoutValue:TextField = null;
  @FXML private var cpuThrottlingSet:CheckBox = null;
  @FXML private var cpuThrottlingValue:TextField = null;

  private var appConfig:Option[AppConfig] = None
  def edit(appConf:AppConfig):Unit = {
    appConfig = Some(appConf)
    appConf.dockerConnect match {
      case DockerConnect.UnixSocketFile(fileName, socketReadTimings, dockerLogger, httpLogger) => {
        unixSocket.setText(fileName)
        socketReadTimings match {
          case Some(timings) =>
            readTimeoutSet.setSelected(timings.readTimeout.isDefined)
            readTimeoutValue.setText(timings.readTimeout.map(_.toString()).getOrElse(""))
            sourceTimeoutSet.setSelected(timings.sourceTimeout.isDefined)
            sourceTimeoutValue.setText(timings.sourceTimeout.map(_.toString()).getOrElse(""))
            cpuThrottlingSet.setSelected(timings.cpuThrottling.isDefined)
            cpuThrottlingValue.setText(timings.cpuThrottling.map(_.toString()).getOrElse(""))
          case None =>
            readTimeoutSet.setSelected(false)
            readTimeoutValue.setText("")
            sourceTimeoutSet.setSelected(false)
            sourceTimeoutValue.setText("")
            cpuThrottlingSet.setSelected(false)
            cpuThrottlingValue.setText("")
        }
      }
    }
  }

  def read:Option[AppConfig] = {
    appConfig.map { ac =>
      ac.dockerConnect match {
        case DockerConnect.UnixSocketFile(fileName, socketReadTimings, dockerLogger, httpLogger) =>
          ac.copy(
            dockerConnect = DockerConnect.UnixSocketFile(
              fileName = unixSocket.getText,
              socketReadTimings = Some(SocketReadTimings(
                cpuThrottling = if( cpuThrottlingSet.isSelected ) None
                  else Duration.parse(cpuThrottlingValue.getText).map(Some(_)).getOrElse(socketReadTimings.flatMap(_.cpuThrottling)),

                readTimeout = if( readTimeoutSet.isSelected ) None
                  else Duration.parse(readTimeoutValue.getText).map(Some(_)).getOrElse(socketReadTimings.flatMap(_.readTimeout)),

                sourceTimeout = if( sourceTimeoutSet.isSelected ) None
                  else Duration.parse(sourceTimeoutValue.getText).map(Some(_)).getOrElse(socketReadTimings.flatMap(_.sourceTimeout))
              )),
              dockerLogger = dockerLogger,
              httpLogger = httpLogger
            )
          )
      }
    }
  }

  def save():Unit = {
    read.foreach { conf =>
      AppHome.writeConfig(conf).left.foreach{ err => println(err) }
    }
  }
}

object AppConfigController {
  def show():Unit = {
    val loader = new FXMLLoader()
    loader.setLocation(this.getClass.getResource("/xyz/cofe/lima/ui/app-config.fxml"))

    val parent = loader.load[Parent]()
    val controller = loader.getController[AppConfigController]

    val stage = new Stage()
    stage.setTitle("Application config")

    val scene = new Scene(parent)
    stage.setScene(scene)
    stage.show()

    AppHome.readConfig.foreach(conf => controller.edit(conf))
  }
}