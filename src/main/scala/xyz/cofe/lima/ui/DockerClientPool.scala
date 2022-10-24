package xyz.cofe.lima.ui

import xyz.cofe.lima.docker.DockerClient

import java.util.concurrent.{Executors, ThreadFactory, ThreadPoolExecutor}

class DockerClientPool( val dockerClient:DockerClient ) {
  lazy val pool = {
    val thPool = Executors.newCachedThreadPool(new ThreadFactory {
      override def newThread(r: Runnable): Thread = {
        val th = new Thread(r)
        th.setDaemon(true)
        th
      }
    })
    thPool
  }

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
