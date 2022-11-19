package xyz.cofe.lima.ui

import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.fxml.FXML
import javafx.scene.control._
import javafx.scene.input.KeyCode

class AppConfigController1234 {
  import AppConfigController1234._
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
      val str = param.getValue.getValue match {
        case StringProp(name, value) => value
      }

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
        event.getRowValue.getValue match {
          case prop @ StringProp(name, value) =>
            prop.value = event.getNewValue
        }
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
      new CF1()
    }


    ///////////////////////

    val rootItem:TreeItem[Prop] = new TreeItem(StringProp("root"))
    val strItem1:TreeItem[Prop] = new TreeItem(StringProp("str1","sample value"))
    val strItem2:TreeItem[Prop] = new TreeItem(StringProp("str2","sample value"))
    val strItem3:TreeItem[Prop] = new TreeItem(StringProp("str3","sample value"))
    rootItem.getChildren.add(strItem1)
    rootItem.getChildren.add(strItem2)
    rootItem.getChildren.add(strItem3)

    treeView.setRoot(rootItem)
    treeView.setShowRoot(false)
  }

  def save():Unit = {
  }
}

object AppConfigController1234 {
  sealed trait Prop {
    def name: String
  }

  case class StringProp(name: String, var value:String="some") extends Prop

  class CF1() extends TreeTableCell[Prop,String]() {
    var textField:Option[TextField] = None
    override def startEdit(): Unit = {
      println(s"startEdit")
      super.startEdit()

      val cell = this

      val txtField = textField match {
        case Some(value) =>
          println( "getText="+ getText )
          value
        case None =>
          textField = Some({
            val txtField = new TextField("")
            txtField.setOnAction(ev => {
              println("commit!")
              cell.commitEdit(txtField.getText)
              ev.consume()
            })
            txtField.setOnKeyReleased(ev => {
              if (ev.getCode == KeyCode.ESCAPE) {
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

      cell.getTableRow.getTreeItem.getValue match {
        case StringProp(name, propValue) =>
          println(s"name=$name propValue=$propValue")
          txtField.setText(propValue)
      }

      setGraphic(txtField)
      txtField.selectAll()
      txtField.requestFocus()
    }

    override def cancelEdit(): Unit = {
      super.cancelEdit()
      setGraphic(null)
    }

    override def updateItem(item: String, empty: Boolean): Unit = {
      super.updateItem(item, empty)
      if (isEmpty) {
        setText(null)
        setGraphic(null)
      } else {
        if (isEditing) {
          textField.foreach(tf => tf.setText(item))
          setText(null)
          if (getGraphic != null) {
            setGraphic(textField.orNull)
          }
        } else {
          setText(item)
          setGraphic(null)
        }
      }
    }
  }
}
