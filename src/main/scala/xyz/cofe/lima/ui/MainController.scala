package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control.RadioMenuItem
import xyz.cofe.lima.docker.DockerClient

class MainController {
  @FXML private var containersController : ContainersController = null
  @FXML private var imagesController : ImagesController = null
  @FXML private var noRefresh:RadioMenuItem = null
  @FXML private var refreshEach1Sec:RadioMenuItem = null
  @FXML private var refreshEach3Sec:RadioMenuItem = null
  @FXML private var refreshEach10Sec:RadioMenuItem = null
  @FXML private var refreshEach60Sec:RadioMenuItem = null

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
    if( containersController!=null ) containersController.setDockerClient(dc)
    if( imagesController!=null ) imagesController.setDockerClient(dc)
  }

  private var changeTimerPeriod:Int=>Unit = _ => ()
  def changeTimerPeriodCallback( call:Int=>Unit ):Unit = {
    changeTimerPeriod = call
  }
  def onTimerPeriod( periodInSeconds:Int ):Unit = {
    periodInSeconds match {
      case _ if periodInSeconds<=0 => noRefresh.setSelected(true)
      case _ if periodInSeconds<3 => refreshEach1Sec.setSelected(true)
      case _ if periodInSeconds<10 => refreshEach3Sec.setSelected(true)
      case _ if periodInSeconds<60 => refreshEach10Sec.setSelected(true)
      case _ => refreshEach60Sec.setSelected(true)
    }
  }

  def refreshByTimer():Unit = {
    containersController.refreshByTimer()
    imagesController.refreshByTimer()
  }

  def setNoRefresh():Unit = changeTimerPeriod(0)
  def setRefreshEach1Sec():Unit = changeTimerPeriod(1)
  def setRefreshEach3Sec():Unit = changeTimerPeriod(3)
  def setRefreshEach10Sec():Unit = changeTimerPeriod(10)
  def setRefreshEach60Sec():Unit = changeTimerPeriod(60)
}
