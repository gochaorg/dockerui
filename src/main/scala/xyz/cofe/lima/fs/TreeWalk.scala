package xyz.cofe.lima.fs

import java.nio.file.Path
import xyz.cofe.lima.fs.syntax._

class TreeWalk( var workSet:List[Path] )(implicit
                                         openOptions: OpenOptions,
                                         linkOptions: LinkOptions,
                                         copyOptions: CopyOptions,
                                         trace: JavaNioTracer) extends Iterator[Path] {
  override def hasNext: Boolean = workSet.nonEmpty
  override def next(): Path = {
    val res = workSet.head
    res.isDir.foreach{ isDir =>
      if( isDir ){
        res.readDir.foreach { subFiles =>
          workSet = subFiles ++ workSet
        }
      }
    }
    res
  }
}
