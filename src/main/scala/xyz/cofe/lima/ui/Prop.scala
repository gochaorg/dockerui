package xyz.cofe.lima.ui

import javafx.scene.control.TreeItem
import TreeWriter.{TreeLeaf, TreeNode}

/**
 * Пара описывающая ключ-значение - свойство
 * @param name имя свойства
 * @param value значение свойства
 */
case class Prop(name:String, value:String) {
  def getName():String = name
  def setName(str:String):Unit = ()

  def getValue():String = value
  def setValue(str:String):Unit = ()
}

object Prop {
  def tree(treeNode: TreeNode, clearNode:Boolean=true):TreeItem[Prop] = {
    val tnodeRoot = if( clearNode ) {
      treeNode.cloneNode()
      treeNode.clear
      treeNode
    }else{
      treeNode
    }

    tnodeRoot match {
      case TreeWriter.TreeLeaf(name) => throw new IllegalArgumentException("!")
      case tnParent:TreeWriter.TreeParent =>
        val tnValueOpt = treeNode.children.find { n => n.isInstanceOf[TreeLeaf] }.map(_.asInstanceOf[TreeLeaf])
        val tiNode = tnValueOpt match {
          case Some(tnValue) => new TreeItem[Prop](Prop(tnParent.string,tnValue.string))
          case None => new TreeItem[Prop](Prop(tnParent.string,""))
        }
        tnParent.children.foreach {
          case _: TreeWriter.TreeLeaf =>
          case tnChild@(_: TreeWriter.TreeParent) =>
            val tiChild = tree(tnChild, clearNode = false)
            tiNode.getChildren.add(tiChild)
        }
        tiNode
    }
  }

  def apply[T:TreeShow](obj:T): Option[TreeItem[Prop]] = {
    val collector = TreeWriter.Writer()
    val showTree = implicitly[TreeShow[T]]
    showTree.show(collector,obj)
    collector.current.map(tn => Prop.tree(tn))
  }
}