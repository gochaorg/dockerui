package xyz.cofe.lima.fs

import java.nio.file.CopyOption

trait CopyOptions {
  def options:Seq[CopyOption]
}

object CopyOptions {
  implicit val defaultOptions: CopyOptions = new CopyOptions {
    override def options: Seq[CopyOption] = List()
  }
}