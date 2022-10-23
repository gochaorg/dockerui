package xyz.cofe.lima.store.log

import xyz.cofe.lima.fs.{CopyOptions, JavaNioTracer, LinkOptions, OpenOptions}

import java.nio.file.Path
import xyz.cofe.lima.fs.syntax._

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
}
