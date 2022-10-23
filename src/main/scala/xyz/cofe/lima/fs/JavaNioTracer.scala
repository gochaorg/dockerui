package xyz.cofe.lima.fs

import java.nio.file.Path
import java.nio.file.LinkOption
import java.nio.charset.Charset

sealed trait JavaNioOperation {
  type R
}

object JavaNioOperation {
  case class IsDirectory(path:Path, linkOptions:Seq[LinkOption]) extends JavaNioOperation { type R = Boolean }
  case class ReadDir(path:Path, linkOptions:Seq[LinkOption]) extends JavaNioOperation { type R = List[Path] }
  case class Exists(path:Path, linkOptions:Seq[LinkOption]) extends JavaNioOperation { type R = Boolean }
  case class IsFile(path:Path, linkOptions:Seq[LinkOption]) extends JavaNioOperation { type R = Boolean }
  case class FileSize(path:Path) extends JavaNioOperation { type R = Long }

  case class ReadString(path:Path, cs:Charset) extends JavaNioOperation { type R = String }
  case class WriteString(path:Path, cs:Charset, str:String) extends JavaNioOperation { type R = Unit }
  case class AppendString(path:Path, cs:Charset, str:String) extends JavaNioOperation { type R = Unit }
  case class CreateDirectories(path:Path) extends JavaNioOperation { type R = Unit }
  case class Move(from:Path, to:Path, options:Seq[java.nio.file.CopyOption]) extends JavaNioOperation { type R = Unit }
  case class Delete(path:Path) extends JavaNioOperation { type R = Unit }
  case class DeleteIfExists(path:Path) extends JavaNioOperation { type R = Unit }
  case class Writer(path:Path, charset:Charset, options:Seq[java.nio.file.OpenOption]) extends JavaNioOperation { type R = java.io.Writer }
  case class AppendWriter(path:Path, charset:Charset, options:Seq[java.nio.file.OpenOption]) extends JavaNioOperation { type R = java.io.Writer }
}

trait JavaNioTracer {
  def apply[O <: JavaNioOperation](op: O)(code: => op.R): op.R
  def error[O <: JavaNioOperation, E <: Throwable](op: O)(error: E): E = error
}

object JavaNioTracer {
  implicit val defaultTracer: JavaNioTracer = new JavaNioTracer {
    override def apply[O <: JavaNioOperation](op: O)(code: => op.R): op.R = code
  }
}
