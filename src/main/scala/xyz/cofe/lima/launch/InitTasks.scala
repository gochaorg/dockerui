package xyz.cofe.lima.launch

/**
 * Фоновые задачи
 */
trait InitTasks {
  /**
   * добавляет хадачу очстки логов
   * @param runnable задача
   */
  def addLogCleanTask(runnable: Runnable)
}

object InitTasks {
  implicit val defaultInstance = new InitTasks {
    override def addLogCleanTask(runnable: Runnable): Unit = {}
  }
}