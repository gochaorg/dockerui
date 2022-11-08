package xyz.cofe.lima.store.log

class AppendableJoin( val outputs:Seq[Appendable] ) extends Appendable with AutoCloseable {
  override def append(csq: CharSequence): Appendable = {
    outputs.foreach(out => out.append(csq))
    this
  }
  override def append(csq: CharSequence, start: Int, end: Int): Appendable = {
    outputs.foreach(out => out.append(csq,start,end))
    this
  }
  override def append(c: Char): Appendable = {
    outputs.foreach(out => out.append(c))
    this
  }
  override def close(): Unit = {
    outputs.foreach { case (out) =>
      out match {
        case c:AutoCloseable => c.close()
        case _ => ()
      }
    }
  }
}

object AppendableJoin {
  def apply(outputs:Appendable*):AppendableJoin = new AppendableJoin(outputs)
}
