package xyz.cofe.lima.fs

import tethys.{JsonReader, JsonWriter}
import xyz.cofe.lima.fs.JavaNioOperation._

import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, OpenOption, Path, StandardOpenOption}

object syntax {
  implicit class PathOps(path: Path) {
    def isDir(implicit trace: JavaNioTracer, lopt: LinkOptions): Either[Throwable, Boolean] = {
      val lOps = lopt.options
      val op = IsDirectory(path, lOps)
      try {
        Right(trace(op)(Files.isDirectory(path, lOps: _*)))
      } catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def readDir(implicit trace: JavaNioTracer, lopt: LinkOptions): Either[Throwable, List[Path]] = {
      val lOps = lopt.options
      val op = ReadDir(path, lOps)
      try
        Right(trace(op) {
          val ds = Files.newDirectoryStream(path)
          val itr = ds.iterator()
          var ls = List[Path]()
          while (itr.hasNext) {
            ls = itr.next() :: ls
          }
          ds.close()
          ls.reverse
        })
      catch {
        case e: Throwable =>
          Left(trace.error(op)(e))
      }
    }

    def parent: Option[Path] = {
      path.getParent match {
        case null => None
        case prnt: Path => Some(prnt)
      }
    }

    def name: String = {
      path.getFileName.toString
    }

    def extension: Option[String] = {
      val idx = name.lastIndexOf(".")
      if (idx >= 0 && idx < (name.length() - 1))
        Some(name.substring(idx + 1))
      else
        None
    }

    def exists(implicit trace: JavaNioTracer, lopt: LinkOptions): Either[Throwable, Boolean] = {
      val lOps = lopt.options
      val op = Exists(path, lOps)
      try
        Right(trace(op)(Files.exists(path, lOps: _*)))
      catch {
        case e: Throwable =>
          Left(trace.error(op)(e))
      }
    }

    def isFile(implicit trace: JavaNioTracer, lopt: LinkOptions): Either[Throwable, Boolean] = {
      val lOps = lopt.options
      val op = IsFile(path, lOps)
      try
        Right(trace(op)(Files.isRegularFile(path, lOps: _*)))
      catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def fileSize(implicit trace: JavaNioTracer): Either[Throwable, Long] = {
      val op = FileSize(path)
      try
        Right(trace(op)(Files.size(path)))
      catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def canonical: Path = path.toAbsolutePath.normalize()

    def upPath: List[Path] = {
      var ls = List[Path]()
      var p = path.canonical
      var stop = false
      while (!stop) {
        ls = p :: ls
        p.parent match {
          case Some(prnt) => p = prnt
          case None => stop = true
        }
      }
      ls
    }

    def readString(cs: Charset)(implicit trace: JavaNioTracer):Either[Throwable, String] = {
      val op = ReadString(path, cs)
      try
        Right(trace(op)(Files.readString(path, cs)))
      catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def writeString(cs: Charset, str: String)(implicit trace: JavaNioTracer): Either[Throwable, Unit] = {
      val op = WriteString(path, cs, str)
      try
        Right(
          trace(op) {
            val _ = Files.writeString(path, str, cs)
          }
        )
      catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def appendString(cs: Charset, str: String)(implicit trace: JavaNioTracer):Either[Throwable, Unit] = {
      val op = AppendString(path, cs, str)
      try {
        Right(
          trace(op) {
            val stream = Files.newOutputStream(path, Array(StandardOpenOption.CREATE, StandardOpenOption.APPEND): _*)
            try {
              stream.write(str.getBytes(cs))
              stream.flush()
            } finally {
              stream.close()
            }
          }
        )
      }catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def createDirectories(implicit trace: JavaNioTracer):Either[Throwable, Unit] = {
      val op = CreateDirectories(path)
      try
        Right(
          trace(op) {
            val _ = Files.createDirectories(path)
          }
        )
      catch {
        case e: Throwable =>
          Left(trace.error(op)(e))
      }
    }

    def move(target: Path)(implicit copt: CopyOptions, trace: JavaNioTracer):Either[Throwable, Unit] = {
      val op = Move(path,target,copt.options)
      try {
        Right(
          trace(op)(Files.move(path, target, op.options: _*))
        )
      } catch {
        case e:Throwable => Left(trace.error(op)(e))
      }
    }

    def delete(implicit trace: JavaNioTracer):Either[Throwable,Unit] = {
      val op = Delete(path)
      try {
        Right(
          trace(op){
            Files.delete(path)
          }
        )
      } catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def writer(cs: Charset)(implicit opt: OpenOptions, trace: JavaNioTracer):Either[Throwable,_ <: java.io.Writer] = {
      val op = Writer(path,cs,opt.options)
      try {
        Right {
          trace(op) {
            Files.newBufferedWriter(path, cs, op.options: _*)
          }
        }
      } catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    def appendWriter(cs: Charset)(implicit opt: OpenOptions, trace: JavaNioTracer):Either[Throwable,_ <: java.io.Writer] = {
      val opts = opt.append.write.create.options
      val op = Writer(path,cs,opt.options)
      try {
        Right {
          trace(op) {
            Files.newBufferedWriter(path, cs, op.options: _*)
          }
        }
      } catch {
        case e: Throwable => Left(trace.error(op)(e))
      }
    }

    object json {
      def read[A:JsonReader]:Either[Throwable, A] = {
        import tethys._
        import tethys.jackson._
        readString(StandardCharsets.UTF_8).flatMap(str => str.jsonAs[A])
      }

      def write[A:JsonWriter](a:A): Either[Throwable, Unit] = {
        import tethys._
        import tethys.jackson._
        writeString(StandardCharsets.UTF_8, a.asJson)
      }
    }
  }
}