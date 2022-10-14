package xyz.cofe.lima.ui

import javafx.scene.control.TableView
import scala.collection.JavaConverters._

case class SyncTable[A,I](table:TableView[A], identify:A=>I, equ:(A,A)=>Boolean) {
  def sync(newValues:List[A]):Unit = {
    val selected = selectedItems()

    val oldItemsMap = table.getItems.asScala.map(e=>(identify(e),e)).toMap
    val newItemsMap = newValues.map(e=>(identify(e),e)).toMap

    val (removeMap,_) = oldItemsMap.partition { case(oldId,_) => !newItemsMap.contains(oldId) }
    val (newMap,_) = newItemsMap.partition { case(newId,_) => !oldItemsMap.contains(newId) }
    val sameMap = newItemsMap
      .partition { case(newId,_) => oldItemsMap.contains(newId) }
      ._1.filter { case(id,_) => !newMap.contains(id) }
      .map { case(newId,newItm) => (oldItemsMap(newId),newItm) }
    val changed = sameMap.filter { case(oldItm,newItm) => !equ(oldItm,newItm) }

    changed.foreach { case(oldItm,newItm) =>
      val idx = table.getItems.indexOf(oldItm)
      if( idx>=0 )table.getItems.set(idx,newItm)
    }

    removeMap.foreach { case(_,oldItm) => table.getItems.remove(oldItm) }
    newMap.foreach { case(_,newItm) => table.getItems.add(newItm) }

    setSelectedItems(selected)
  }
  def selectedItems():Set[I] = {
    table.getSelectionModel.getSelectedItems.asScala.map(identify).toSet
  }
  def setSelectedItems(selection:Set[I]):Unit = {
    val (mustSelected,mustUnSelected) = table.getItems.asScala.partition(itm => selection.contains(identify(itm)))
    table.getSelectionModel.getSelectedItems.removeAll(mustUnSelected.asJava)
    mustSelected.foreach { itm => table.getSelectionModel.select(itm) }
  }
}
