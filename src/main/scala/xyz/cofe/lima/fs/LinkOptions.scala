package xyz.cofe.lima.fs

import java.nio.file.LinkOption

trait LinkOptions {
  def options: Seq[LinkOption]
}

object LinkOptions {
  implicit val defaultOptions: LinkOptions = new LinkOptions {
    override def options: Seq[LinkOption] = List()
  }
}