package xyz.cofe.lima.store.log

import java.io.Writer
import java.nio.charset.Charset
import java.nio.file.Path
import xyz.cofe.lima.fs.syntax._

class AppendableFile( path: Path, charset: Charset ) extends java.lang.Appendable with AutoCloseable {
  @volatile private var closed: Boolean = false
  @volatile private var writer: Option[Writer] = None

  private def write( code: Writer => Unit ):Unit = {
    if( !closed ) {
      writer match {
        case Some(w) =>
          code(w)
          w.flush()
        case None =>
          path.writer(charset).map { w =>
            writer = Some(w)
            code(w)
            w.flush()
          }.left.map( err => throw err )
      }
    }
  }

  override def append(csq: CharSequence): Appendable = {
    this.synchronized {
      write { w =>
        w.append(csq)
      }
      this
    }
  }

  override def append(csq: CharSequence, start: Int, end: Int): Appendable = {
    this.synchronized {
      write { w =>
        w.append(csq,start,end)
      }
    }
    this
  }
  override def append(c: Char): Appendable = {
    this.synchronized {
      write { w =>
        w.append(c)
      }
    }
    this
  }

  override def close(): Unit = {
    this.synchronized {
      writer match {
        case Some(w) =>
          w.close()
          closed = true
        case None =>
      }
    }
  }
}
