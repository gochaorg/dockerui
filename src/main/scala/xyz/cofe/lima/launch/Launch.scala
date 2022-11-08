package xyz.cofe.lima.launch

import javafx.application.Application
import xyz.cofe.lima.ui.Main

object Launch {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }
}
