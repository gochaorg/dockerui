package xyz.cofe.lima.store.log

import xyz.cofe.lima.fs.{CopyOptions, JavaNioTracer, LinkOptions, OpenOptions}

import java.nio.file.Path
import xyz.cofe.lima.fs.syntax._
import xyz.cofe.lima.store.log.PathPattern.PathPattern

object FilesCleaner {
  def clean(path:Path, limitSize:Long)(implicit
                                       openOptions: OpenOptions,
                                       linkOptions: LinkOptions,
                                       copyOptions: CopyOptions,
                                       trace: JavaNioTracer):Unit = {
    require(limitSize>=0)

    val files = path.walk
      .map(f => (f,f.lastModified))
      .filter { case (f,t) => t.isRight }
      .map { case(f,t) => (f,t.map(_.toMillis).getOrElse(0L)) }
      .toList
      .sortBy { case(f,t) => t }
      .map { case(f,t) => (f,f.fileSize) }
      .filter { case(f,s) => s.isRight }
      .map { case(f,s) => (f,s.getOrElse(0L)) }
      .map { case(f,s) => (f,s,f.isFile) }
      .filter { case(f,s,ff) => ff.getOrElse(false) }
      .map { case(f,s,ff) => (f,s) }

    val total = files.map { case(f,s) => s }.sum

    if( total>limitSize ){
      var curSize = total
      val it = files.iterator
      while( curSize>limitSize && it.hasNext ){
        val (f,s) = it.next()
        f.deleteIfExists
        curSize -= s
      }
    }
  }

  case class SubFile(path: Path, size:Long)
  def subFilesByModifyDate(root:Path)(implicit
                                 openOptions: OpenOptions,
                                 linkOptions: LinkOptions,
                                 copyOptions: CopyOptions,
                                 trace: JavaNioTracer): List[SubFile] = {
    root.walk
      .map(f => (f,f.lastModified))
      .filter { case (f,t) => t.isRight }
      .map { case(f,t) => (f,t.map(_.toMillis).getOrElse(0L)) }
      .toList
      .sortBy { case(f,t) => t }
      .map { case(f,t) => (f,f.fileSize) }
      .filter { case(f,s) => s.isRight }
      .map { case(f,s) => (f,s.getOrElse(0L)) }
      .map { case(f,s) => (f,s,f.isFile) }
      .filter { case(f,s,ff) => ff.getOrElse(false) }
      .map { case(f,s,ff) => SubFile(f,s) }
  }

  def subFilesByModifyDate(pathPattern:PathPattern)(implicit
                                             openOptions: OpenOptions,
                                             linkOptions: LinkOptions,
                                             copyOptions: CopyOptions,
                                             trace: JavaNioTracer): Either[String, List[SubFile]] = {
    pathPattern.pathFilter.flatMap { pathFilter =>
      pathPattern.headPath.map { root =>
        subFilesByModifyDate(root).filter { sfile =>
          pathFilter(sfile.path)
        }
      }
    }
  }

  def clean(path:PathPattern, limitSize:Long)(implicit
                                              openOptions: OpenOptions,
                                              linkOptions: LinkOptions,
                                              copyOptions: CopyOptions,
                                              trace: JavaNioTracer): Either[String, List[Path]] = {
    require(limitSize>=0)

    subFilesByModifyDate(path).flatMap { files =>
      val totalSize = files.map( _.size ).sum
      if( totalSize>limitSize ){
        var curSize = totalSize
        val it = files.iterator
        var deleted = List[Path]()
        while( curSize>limitSize && it.hasNext ){
          val sf = it.next()
          sf.path.deleteIfExists
          curSize -= sf.size
          deleted = deleted :+ sf.path
        }
        Right(deleted)
      }else{
        Right(List())
      }
    }
  }
}
