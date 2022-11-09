package xyz.cofe.lima.ui

import javafx.fxml.FXML
import javafx.scene.control.{CheckBox, TextField}

class AppConfigController {
  @FXML private var unixSocket:TextField = null;
  @FXML private var readTimeoutSet:CheckBox = null;
  @FXML private var readTimeoutValue:TextField = null;
  @FXML private var sourceTimeoutSet:CheckBox = null;
  @FXML private var sourceTimeoutValue:TextField = null;
  @FXML private var cpuThrottlingSet:CheckBox = null;
  @FXML private var cpuThrottlingValue:TextField = null;
  //def edit()

  def save():Unit = {
  }
}
