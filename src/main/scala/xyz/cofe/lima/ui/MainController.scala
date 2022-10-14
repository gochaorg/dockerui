package xyz.cofe.lima.ui

import javafx.fxml.FXML
import xyz.cofe.lima.docker.DockerClient

class MainController {
  @FXML private var containersController : ContainersController = null;
  @FXML private var imagesController : ImagesController = null;

  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
    if( containersController!=null ) containersController.setDockerClient(dc)
    if( imagesController!=null ) imagesController.setDockerClient(dc)
  }

  def refreshByTimer():Unit = {
    containersController.refreshByTimer()
  }
}
