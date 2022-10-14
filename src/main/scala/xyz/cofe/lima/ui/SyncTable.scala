package xyz.cofe.lima.ui

import javafx.collections.ListChangeListener
import javafx.scene.control.TableView
import xyz.cofe.lima.docker.model.ContainerStatus

import scala.collection.JavaConverters._

case class SyncTable[A,I](table:TableView[A], identify:A=>I, equ:(A,A)=>Boolean) {
  var syncRunning=false
  def sync(newValues:List[A]):Unit = {
    try {
      syncRunning = true

      val selected = selectedItems()

      val oldItemsMap = table.getItems.asScala.map(e => (identify(e), e)).toMap
      val newItemsMap = newValues.map(e => (identify(e), e)).toMap

      val (removeMap, _) = oldItemsMap.partition { case (oldId, _) => !newItemsMap.contains(oldId) }
      val (newMap, _) = newItemsMap.partition { case (newId, _) => !oldItemsMap.contains(newId) }
      val sameMap = newItemsMap
        .partition { case (newId, _) => oldItemsMap.contains(newId) }
        ._1.filter { case (id, _) => !newMap.contains(id) }
        .map { case (newId, newItm) => (oldItemsMap(newId), newItm) }
      val changed = sameMap.filter { case (oldItm, newItm) => !equ(oldItm, newItm) }

      changed.foreach { case (oldItm, newItm) =>
        val idx = table.getItems.indexOf(oldItm)
        if (idx >= 0) table.getItems.set(idx, newItm)
      }

      removeMap.foreach { case (_, oldItm) => table.getItems.remove(oldItm) }
      newMap.foreach { case (_, newItm) => table.getItems.add(newItm) }

      setSelectedItems(selected)
    } finally {
      syncRunning = false
    }
  }
  def selectedItems():Set[I] = {
    table.getSelectionModel.getSelectedItems.asScala.map(identify).toSet
  }
  def setSelectedItems(selection:Set[I]):Unit = {
    val (mustSelected,mustUnSelected) = table.getItems.asScala.partition(itm => selection.contains(identify(itm)))
    table.getSelectionModel.clearSelection()
    mustSelected.foreach { itm => table.getSelectionModel.select(itm) }
  }

  private var listeners = List[A=>Unit]()
  private def initListeners():Unit = {
    table.getSelectionModel.getSelectedItems.addListener(new ListChangeListener[A] {
      override def onChanged(c: ListChangeListener.Change[_ <: A]): Unit = {
        if (!syncRunning) {
          while (c.next()) {
            if (c.wasPermutated()
              ||  c.wasAdded()
              ||  c.wasUpdated()
              ||  c.wasRemoved()
              ||  c.wasReplaced()
            ) {
              c.getList.asScala.headOption.foreach(itm=>
                listeners.foreach(ls => ls(itm))
              )
            }
          }
        }
      }
    })
  }
  initListeners()

  def trackFocused(onFocus:A=>Unit): SyncTable[A, I] ={
    listeners = onFocus :: listeners
    this
  }
}
