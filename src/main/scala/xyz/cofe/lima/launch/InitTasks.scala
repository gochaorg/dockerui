package xyz.cofe.lima.launch

import java.util.concurrent.{ExecutorService, Executors, ThreadFactory}

/**
 * Фоновые задачи
 */
trait InitTasks {
  /**
   * добавляет задачу очстки логов
   * @param runnable задача
   */
  def addLogCleanTask(runnable: Runnable)
}

object InitTasks {
  implicit val defaultInstance: InitTasks = new InitTasks {
    val executorService = Executors.newCachedThreadPool(new ThreadFactory {
      override def newThread(r: Runnable): Thread = {
        val th = new Thread(r)
        th.setDaemon(true)
        th.setName("cleanup log")
        th
      }
    })

    override def addLogCleanTask(runnable: Runnable): Unit = {
      executorService.submit(runnable)
    }
  }
}