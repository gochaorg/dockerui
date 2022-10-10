package xyz.cofe.lima

import language.experimental.macros, magnolia1._

trait TreeWriter {
  def begin(label:String):Unit
  def end():Unit
  def write(string:String):Unit
}

object TreeWriter {
  val dummy:TreeWriter = new TreeWriter {
    override def begin(label: String): Unit = ()
    override def end(): Unit = ()
    override def write(string: String): Unit = ()
  }

  sealed trait TreeNode {
    def cloneNode():TreeNode
    def children:List[TreeNode] = List()
    def toString(indent:String):String
    def iterator:Iterator[List[TreeNode]] = new Iterator[List[TreeNode]] {
      private var workSet = List[List[TreeNode]](
        List(TreeNode.this)
      )
      override def hasNext: Boolean = workSet.nonEmpty
      override def next(): List[TreeNode] = {
        val res = workSet.head
        workSet = workSet.tail
        res.headOption match {
          case Some(headTreeNode) =>
            workSet = headTreeNode.children.map { child =>
              child :: res
            } ::: workSet
          case None =>
        }
        res
      }
    }
    def leafNodes = iterator.filter { revPath => revPath.headOption.exists { n => n.children.isEmpty } }
    def clear:TreeNode = {
      var nodes = leafNodes.toList
      while( nodes.nonEmpty ){
        val revPath = nodes.head
        revPath.headOption match {
          case Some(leaf:TreeLeaf) =>
            nodes = nodes.filterNot( p=> p==revPath )
          case Some(leaf:TreeNode) => leaf.children.isEmpty match {
            case false => nodes = nodes.filterNot( p=> p==revPath )
            case true => revPath.tail.headOption match {
              case Some(parent:TreeParent) =>
                parent.remove(leaf)
                nodes = nodes.filterNot( p=> p==revPath )
              case Some(broken:TreeLeaf) => nodes = nodes.filterNot( p=> p==revPath )
              case None => nodes = nodes.filterNot( p=> p==revPath )
            }
          }
        }
      }
      this
    }
  }

  case class TreeLeaf(string:String) extends TreeNode {
    override def cloneNode():TreeNode = TreeLeaf(string)
    override def toString(indent: String): String =
      s"${indent}TreeLeaf($string)"
  }
  case class TreeParent(string:String) extends TreeNode {
    private var values = List[TreeNode]()
    override def cloneNode():TreeNode = {
      val prnt =  TreeParent(string)
      prnt.values = values.map(_.cloneNode())
      prnt
    }
    override def children: List[TreeNode] = values
    def append(node:TreeNode):Unit = {
      values = node :: values
    }
    def remove(node:TreeNode):Unit = {
      values = values.filterNot(n => n.eq(node))
    }
    override def toString: String = {
      toString("")
    }
    override def toString(indent:String):String = {
      val sb = new StringBuilder
      sb ++= s"${indent}TreeParent($string)"
      if( values.nonEmpty ){
        sb ++= ":\n"
        sb ++= values.map(_.toString(indent+"  ")).mkString("\n")
      }
      sb.toString()
    }
  }

  case class Writer() extends TreeWriter {
    var roots = List[TreeNode]()
    var current = None:Option[TreeNode]
    var stack = List[TreeNode]()

    def currentParent:TreeParent = {
      current match {
        case Some(prnt:TreeParent) =>
          prnt
        case Some(node:TreeLeaf) =>
          val prnt = TreeParent("?prnt")
          prnt.append(node)
          current = Some(prnt)
          roots = prnt :: roots.filterNot(n => n.eq(node))
          prnt
        case None =>
          val prnt = TreeParent("?prnt")
          current = Some(prnt)
          roots = current.get :: roots
          prnt
      }
    }
    override def begin(label: String): Unit = {
      val prnt = TreeParent(label)
      current match {
        case Some(node:TreeParent) =>
          node.append(prnt)
          stack = node :: stack
          current = Some(prnt)
        case Some(node:TreeLeaf) =>
          stack = node :: stack
          current = Some(prnt)
        case None =>
          val prnt = TreeParent(label)
          current = Some(prnt)
      }
    }
    override def end(): Unit = {
      stack.headOption match {
        case Some(node) =>
          current = Some(node)
          stack = stack.tail
          if( stack.isEmpty ){
            roots = node :: roots
          }
        case None =>
      }
    }
    override def write(string: String): Unit = {
      currentParent.append(TreeLeaf(string))
    }
  }
}

trait TreeShow[T] {
  def show(writer:TreeWriter, value:T):Unit
}

object TreeShowDerivation {
  type Typeclass[T] = TreeShow[T]

  def join[T](ctx: CaseClass[TreeShow, T]): TreeShow[T] = new TreeShow[T] {
    override def show(writer: TreeWriter, value: T): Unit = {
      ctx.parameters.foreach { p =>
        writer.begin(p.label)
        p.typeclass.show(writer,p.dereference(value))
        writer.end()
      }
    }
  }

  def split[T](ctx: SealedTrait[TreeShow, T]): TreeShow[T] =
    new TreeShow[T] {
      override def show(writer: TreeWriter, value: T): Unit = {
        ctx.split(value) { sub =>
          sub.typeclass.show(writer, sub.cast(value))
        }
      }
    }

  implicit def gen[T]: TreeShow[T] = macro Magnolia.gen[T]
}

object TreeShow {
  implicit val intShow: TreeShow[Int] = (w:TreeWriter,t:Int)=>w.write(t.toString)
  implicit val longShow: TreeShow[Long] = (w:TreeWriter,t:Long)=>w.write(t.toString)
  implicit val doubleShow: TreeShow[Double] = (w:TreeWriter,t:Double)=>w.write(t.toString)
  implicit val stringShow: TreeShow[String] = (w:TreeWriter,t:String)=>w.write(t.toString)
  implicit val boolShow: TreeShow[Boolean] = (w:TreeWriter,t:Boolean)=>w.write(t.toString)
  implicit def optShow[T:TreeShow]: TreeShow[Option[T]] = (w:TreeWriter,t:Option[T]) => {
    t match {
      case Some(value) => implicitly[TreeShow[T]].show(w,value)
      case None =>
    }
  }
  implicit def mapShow[K:TreeShow,V:TreeShow]: TreeShow[Map[K,V]] = (w:TreeWriter,t:Map[K,V]) => {
    if(t.nonEmpty) {
      w.begin(":map")
      t.foreach { case (k, v) =>
        w.begin(":key")
        implicitly[TreeShow[K]].show(w, k)
        w.end()
        w.begin(":value")
        implicitly[TreeShow[V]].show(w, v)
        w.end()
      }
      w.end()
    }
  }
  implicit def listShow[V:TreeShow]: TreeShow[List[V]] = (w:TreeWriter,t:List[V]) => {
    if(t.nonEmpty) {
      w.begin(":list")
      t.foreach { case (v) =>
        w.begin(":value")
        implicitly[TreeShow[V]].show(w, v)
        w.end()
      }
      w.end()
    }
  }

  implicit val optMapStr = new TreeShow[Option[Map[String,String]]] {
    override def show(w: TreeWriter, value: Option[Map[String, String]]): Unit = {
      value match {
        case Some(map) =>
          w.begin(":map")
          map.foreach { case(k,v) =>
            w.begin(":key")
            w.write(k)
            w.end()
            w.begin(":value")
            w.write(v)
            w.end()
          }
          w.end()
        case None =>
      }
    }
  }
  implicit val mapStrListMap = new TreeShow[Map[String,List[Map[String,String]]]] {
    override def show(w: TreeWriter, value: Map[String,List[Map[String,String]]]): Unit = {
      if(value.nonEmpty) {
        w.begin(":map")
        value.foreach { case (k, v) =>
          if( v.nonEmpty ) {
            w.begin(":key")
            w.write(k)
            w.end()
            w.begin(":value")
            w.begin(":list")
            v.foreach { el =>
              mapShow[String, String].show(w, el)
            }
            w.end()
            w.end()
          }
        }
        w.end()
      }
    }
  }

}