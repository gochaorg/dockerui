package xyz.cofe.lima.ui

import javafx.beans.property.SimpleStringProperty
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.event.EventHandler
import javafx.scene.control.cell.TextFieldTreeTableCell
import javafx.scene.control.{TreeTableColumn, TreeTableRow, TreeTableView}
import javafx.util.{Callback, StringConverter}

/**
 * Описывает редактируемое свойство
 *
 * @param name имя свойства
 * @param reader функция чтения
 * @param writer функция записи
 */
case class MutProp(name:String, reader:()=>String, writer:String=>Unit)

object MutProp {
  def initPropTree(treeTableView : TreeTableView[MutProp]):Unit = {
    val nameCol = new TreeTableColumn[MutProp, String]("name")
    nameCol.setCellValueFactory((param: TreeTableColumn.CellDataFeatures[MutProp, String]) => {
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

    val valueCol = new TreeTableColumn[MutProp, String]("value")
    valueCol.setCellValueFactory((param: TreeTableColumn.CellDataFeatures[MutProp, String]) => {
      val str = param.getValue.getValue.reader()
      val strProp = new SimpleStringProperty(str)
      strProp.addListener(new ChangeListener[String] {
        override def changed(observable: ObservableValue[_ <: String], oldValue: String, newValue: String): Unit = {
          println(s"change $oldValue => $newValue")
        }
      })
      strProp
    })
    valueCol.setOnEditCommit(new EventHandler[TreeTableColumn.CellEditEvent[MutProp, String]]() {
      override def handle(event: TreeTableColumn.CellEditEvent[MutProp, String]): Unit = {
        event.getRowValue.getValue.writer(event.getNewValue)
      }
    })
    valueCol.setEditable(true)

    treeTableView.getColumns.clear()
    treeTableView.getColumns.add(nameCol)
    treeTableView.getColumns.add(valueCol)
    treeTableView.setEditable(true)

    treeTableView.setRowFactory { table =>
      new TreeTableRow[MutProp]() {
        override def updateItem(item: MutProp, empty: Boolean): Unit = {
          super.updateItem(item, empty)
          setEditable(true)
        }
      }
    }

    valueCol.setCellFactory { c =>
      new TextFieldTreeTableCell[MutProp, String](new StringConverter[String] {
        override def toString(`object`: String): String = `object`
        override def fromString(string: String): String = string
      }) {
        override def startEdit(): Unit = {
          super.startEdit()
        }
      }
    }
  }
}