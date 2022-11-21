package xyz.cofe.lima.ui

import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.hub.DockerHubClient

import java.util.concurrent.{ExecutorService, Executors}

/**
 * Пул потоков для работы с docker
 * @param buildClient клиент docker
 */
class DockerHubClientPool(buildClient: => DockerHubClient ) {
  lazy val pool: ExecutorService = {
    val thPool = Executors.newCachedThreadPool((r: Runnable) => {
      val th = new Thread(r)
      th.setDaemon(true)
      th.setName(s"docker-hub-${th.getId}")
      th
    })
    thPool
  }

  /**
   * Отправляет задание на выполнение в отдельном потоке
   *
   * @param code код работающий с docker hub client
   */
  def submit(code: DockerHubClient => Unit): Unit = {
    pool.submit(new Runnable {
      override def run(): Unit = {
        code(buildClient)
      }
    })
  }
}

object DockerHubClientPool {
  @volatile private var defaultPool: Option[DockerHubClientPool] = None

  def init(pool: DockerHubClientPool):Unit = {
    this.synchronized {
      if( defaultPool.isDefined )throw new IllegalStateException("already inited")
      defaultPool = Some(pool)
    }
  }

  def submit( code: DockerHubClient=>Unit ):Unit =
    defaultPool.foreach { p => p.submit(code) }
}
