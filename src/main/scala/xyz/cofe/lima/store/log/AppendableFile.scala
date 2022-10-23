package xyz.cofe.lima.store.log

import xyz.cofe.lima.fs.{CopyOptions, JavaNioTracer, LinkOptions, OpenOptions}

import java.io.Writer
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Path
import xyz.cofe.lima.fs.syntax._

class AppendableFile(
                      pathPattern: List[PathPattern.Name],
                      charset: Charset=StandardCharsets.UTF_8,
                      limitSizePerFile:Option[Long]=None
                    )
                    (implicit
                     openOptions: OpenOptions,
                     linkOptions: LinkOptions,
                     copyOptions: CopyOptions,
                     trace: JavaNioTracer)
  extends java.lang.Appendable with AutoCloseable {
  @volatile private var closed: Boolean = false
  @volatile private var writer: Option[Writer] = None
  @volatile private var path: Option[Path] = None

  private def newFile = {
    def open = {
      pathPattern.generate match {
        case Left(err) => throw new Error(s"pattern error $err")
        case Right(path) =>
          path.canonical.parent.foreach { dir => dir.createDirectories }
          this.path = Some(path)
          path.writer(charset).map { w =>
            writer = Some(w)
            w
          }
      }
    }
    writer match {
      case Some(w) =>
        w.close()
        open
      case None =>
        open
    }
  }

  private def write( code: Writer => Unit ):Unit = {
    if( !closed ) {
      writer match {
        case Some(w) =>
          code(w)
          w.flush()
          path.foreach { curFile =>
            limitSizePerFile.foreach { limitSize =>
              curFile.fileSize.foreach { curSize =>
                if( curSize>limitSize ){
                  newFile.left.map { e => throw e }
                }
              }
            }
          }
        case None =>
          newFile.left.map { e => throw e }.map { w =>
            code(w)
            w.flush()
          }
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

object AppendableFile {
  def apply(pathPattern: List[PathPattern.Name],
            charset: Charset = StandardCharsets.UTF_8,
            limitSizePerFile: Option[Long] = None
           )(implicit
             openOptions: OpenOptions,
             linkOptions: LinkOptions,
             copyOptions: CopyOptions,
             trace: JavaNioTracer)
  :AppendableFile = new AppendableFile(
    pathPattern, charset, limitSizePerFile
  )
}
