package xyz.cofe.lima.ui

import xyz.cofe.lima.docker.DockerClient

import java.util.concurrent.{ExecutorService, Executors}

/**
 * Пул потоков для работы с docker
 * @param dockerClient клиент docker
 */
class DockerClientPool( val dockerClient:DockerClient ) {
  lazy val pool: ExecutorService = {
    val thPool = Executors.newCachedThreadPool((r: Runnable) => {
      val th = new Thread(r)
      th.setDaemon(true)
      th
    })
    thPool
  }

  /**
   * Отправляет задание на выполнение в отдельном потоке
   * @param code код работающий с docker client
   */
  def submit( code: DockerClient=>Unit ):Unit = {
    pool.submit(new Runnable {
      override def run(): Unit = {
        code(dockerClient.newClient)
      }
    })
  }
}

object DockerClientPool {
  @volatile private var defaultPool: Option[DockerClientPool] = None
  def init(pool: DockerClientPool):Unit = {
    this.synchronized {
      if( defaultPool.isDefined )throw new IllegalStateException("already inited")
      defaultPool = Some(pool)
    }
  }

  def submit( code: DockerClient=>Unit ):Unit =
    defaultPool.foreach { p => p.submit(code) }
}
