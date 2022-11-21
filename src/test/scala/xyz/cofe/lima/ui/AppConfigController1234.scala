package xyz.cofe.lima.ui

import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.fxml.FXML
import javafx.scene.Node
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
        println("setOnEditCommit handle "+event.getNewValue)
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

  trait Editor[V] {
    def startEdit(value:V, commit:V=>Unit, cancel:()=>Unit):Node
    def cancelEdit():Unit
    def updateItem(value:V, empty:Boolean):Unit
    def requireFocus():Unit = ()
  }

  class StringEditor extends Editor[String] {
    var initial: Option[String] = None
    var commit: Option[String=>Unit] = None
    var cancel: Option[()=>Unit] = None

    val textField : TextField = {
      val field = new TextField()
      field.setOnAction { ev =>
        commit.foreach( f => f(field.getText) )
        ev.consume()
      }
      field.setOnKeyReleased { ev =>
        if( ev.getCode == KeyCode.ESCAPE ){
          println("editor cancelEdit")
          cancel.foreach( f => f() )
          ev.consume()
        }
      }
      field
    }
    override def startEdit(value: String, commit: String => Unit, cancel: () => Unit): Node = {
      this.initial = Some(value)
      this.commit = Some(commit)
      this.cancel = Some(cancel)
      textField.setText(value)
      textField
    }

    override def cancelEdit(): Unit = {
    }

    override def updateItem(value: String, empty: Boolean): Unit = {
      if(empty){
        textField.setText("")
      }else{
        textField.setText(value)
      }
    }

    override def requireFocus():Unit = {
      textField.selectAll()
      textField.requestFocus()
    }
  }

  class CF1() extends TreeTableCell[Prop,String]() {
    var editor:Editor[String] = new StringEditor()
//    var editor = new StringEditor()

    override def startEdit(): Unit = {
      super.startEdit()

      this.setText(null)

      val cell = this

      setGraphic(
        editor.startEdit({
          cell.getTableRow.getTreeItem.getValue match {
            case StringProp(name, propValue) => propValue
          }
        }
          , cell.commitEdit
          , cell.cancelEdit
      ))

      editor.requireFocus()
    }

    override def cancelEdit(): Unit = {
      super.cancelEdit()
      setGraphic(null)
      setText({
        getTableRow.getTreeItem.getValue match {
          case StringProp(name, propValue) => propValue
        }
      })
      editor.cancelEdit()
    }

    override def updateItem(item: String, empty: Boolean): Unit = {
      super.updateItem(item, empty)
      if (isEmpty) {
        setText(null)
        setGraphic(null)
      } else {
        if (isEditing) {
          editor.updateItem(item,empty)
          setText(null)
        } else {
          setText(item)
          setGraphic(null)
        }
      }
    }
  }
}
