package xyz.cofe.lima.ui

import xyz.cofe.lima.docker.DockerClient

class ContainerController {
  private var dockerClient: Option[DockerClient] = None
  def setDockerClient( dc: DockerClient ):Unit = {
    dockerClient = Some(dc)
  }

  def select(containerId:Option[String]):Unit = {}
}
