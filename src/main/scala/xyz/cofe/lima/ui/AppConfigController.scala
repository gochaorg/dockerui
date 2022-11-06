package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control.TreeTableView

class AppConfigController {
  @FXML
  private var params: TreeTableView[MutProp] = null

  @FXML
  def initialize(): Unit = {
    MutProp.initPropTree(params)
  }

  def save():Unit = {
  }
}
