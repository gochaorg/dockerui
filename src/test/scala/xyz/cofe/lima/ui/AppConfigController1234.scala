package xyz.cofe.lima.ui

import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.fxml.FXML
import javafx.scene.control._
import javafx.scene.input.KeyCode

class AppConfigController1234 {
  sealed trait Prop {
    def name:String
  }
  case class StringProp(name:String) extends Prop

  @FXML
  private var treeView: TreeTableView[Prop] = null

  @FXML
  def initialize(): Unit = {
    val nameCol = new TreeTableColumn[Prop, String]("name")
    nameCol.setCellValueFactory((param: TreeTableColumn.CellDataFeatures[Prop, String]) => {
      val str = param.getValue.getValue.name
      val strProp = new SimpleStringProperty(str)
      strProp.addListener(new ChangeListener[String] {
        override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
          println(s"change $oldValue => $newValue")
        }
      })
      strProp
    })
    nameCol.setEditable(false)

    val valueCol = new TreeTableColumn[Prop, String]("value")
    valueCol.setCellValueFactory((param: TreeTableColumn.CellDataFeatures[Prop, String]) => {
      val str = "str123"
      val strProp = new SimpleStringProperty(str)
      strProp.addListener(new ChangeListener[String] {
        override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
          println(s"change $oldValue => $newValue")
        }
      })
      strProp
    })
    valueCol.setOnEditCommit(new EventHandler[TreeTableColumn.CellEditEvent[Prop, String]]() {
      override def handle(event: TreeTableColumn.CellEditEvent[Prop, String]): Unit = {
        println(event.getNewValue)
        //event.getRowValue.getValue.writer(event.getNewValue)
      }
    })
    valueCol.setEditable(true)

    treeView.getColumns.clear()
    treeView.getColumns.add(nameCol)
    treeView.getColumns.add(valueCol)
    treeView.setEditable(true)

    treeView.setRowFactory { table =>
      new TreeTableRow[Prop]() {
        override def updateItem(item: Prop, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          setEditable(true)
        }
      }
    }

    valueCol.setCellFactory { c =>
      println(s"cell factory ${c}")
      new AppConfigController1234.CF("asdf")
    }


    ///////////////////////

    val rootItem:TreeItem[Prop] = new TreeItem(StringProp("root"))
    val strItem:TreeItem[Prop] = new TreeItem(StringProp("str"))
    rootItem.getChildren.add(strItem)

    treeView.setRoot(rootItem)
    treeView.setShowRoot(false)
  }

  def save():Unit = {
  }
}

object AppConfigController1234 {
  class CF[P,S](str:S) extends TreeTableCell[P,S]() {
    var textField:Option[TextField] = None
    override def startEdit(): Unit = {
      println(s"startEdit")
      //(this.asInstanceOf[TreeTableCell[_,_]]).startEdit()
      super.startEdit()

      val cell = this

      val txtField = textField match {
        case Some(value) => value
        case None =>
          textField = Some({
            val txtField = new TextField("")
            txtField.setOnAction(ev => {
              println("commit!")
              cell.commitEdit(str)
              ev.consume()
            })
            txtField.setOnKeyReleased(ev => {
              if( ev.getCode == KeyCode.ESCAPE ){
                println("cancel")
                cell.cancelEdit()
                ev.consume()
              }
            })
            txtField
          })
          textField.get
      }

      this.setText(null)

      txtField.setText("")
      setGraphic(txtField)
      txtField.selectAll()
      txtField.requestFocus()
    }

    override def cancelEdit(): Unit = {
      super.cancelEdit()
      setGraphic(null)
    }

    override def updateItem(item: S, empty: Boolean): Unit = {
      super.updateItem(item,empty)
      if( isEmpty ){
        setText(null)
        setGraphic(null)
      }else{
        if(isEditing){
          textField.foreach(tf => tf.setText("asdasdasd"))
          setText(null)
          if(getGraphic!=null){
            setGraphic(textField.orNull)
          }
        }else{
          setText("cdcdca")
          setGraphic(null)
        }
      }
    }
  }
}
