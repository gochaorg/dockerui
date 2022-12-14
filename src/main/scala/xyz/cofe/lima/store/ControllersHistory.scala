package xyz.cofe.lima.store

import xyz.cofe.lima.docker.log.Logger

object ControllersHistory {
  lazy val createContainerHistory: History[Logger.ContainerCreate] =
    History(AppHome.directory.resolve("history/createContainer.history"), 100)
  lazy val imageCreateHistory: History[Logger.ImageCreate] =
    History(AppHome.directory.resolve("history/imageContainer.history"), 100)
}
