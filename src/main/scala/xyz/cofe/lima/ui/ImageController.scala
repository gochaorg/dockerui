package xyz.cofe.lima.ui

import xyz.cofe.lima.docker.DockerClient

class ImageController {
  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
  }
}
