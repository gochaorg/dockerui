package xyz.cofe.lima.fs

import java.nio.file.OpenOption
import java.nio.file.StandardOpenOption

trait OpenOptions {
  def options:Seq[java.nio.file.OpenOption]
  def clear:OpenOptions
  def append:OpenOptions
  def append(inc:Boolean):OpenOptions
  def create:OpenOptions
  def create(inc:Boolean):OpenOptions
  def createNew:OpenOptions
  def createNew(inc:Boolean):OpenOptions
  def deleteOnClose:OpenOptions
  def deleteOnClose(inc:Boolean):OpenOptions
  def dsync:OpenOptions
  def dsync(inc:Boolean):OpenOptions
  def read:OpenOptions
  def read(inc:Boolean):OpenOptions
  def sparse:OpenOptions
  def sparse(inc:Boolean):OpenOptions
  def sync:OpenOptions
  def sync(inc:Boolean):OpenOptions
  def truncateExisting:OpenOptions
  def truncateExisting(inc:Boolean):OpenOptions
  def write:OpenOptions
  def write(inc:Boolean):OpenOptions
}

object OpenOptions {
  case class OpenOptionsHolder(options:List[java.nio.file.OpenOption]=List()) extends OpenOptions {
    def include(opt: OpenOption):OpenOptionsHolder =
      copy(options = opt :: options.filterNot(_ == opt))

    def exclude(opt: OpenOption):OpenOptionsHolder =
      copy(options = options.filterNot(_ == opt))

    def toggle(opt: OpenOption, inc:Boolean): OpenOptionsHolder =
      if( inc )
        include(opt)
      else
        exclude(opt)

    override def clear:OpenOptionsHolder =
      copy(options=List())

    def append:OpenOptionsHolder = include(StandardOpenOption.APPEND)
    def append(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.APPEND,inc)

    def create:OpenOptionsHolder = include(StandardOpenOption.CREATE)
    def create(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.CREATE,inc)

    def createNew:OpenOptionsHolder = include(StandardOpenOption.CREATE_NEW)
    def createNew(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.CREATE_NEW,inc)

    def deleteOnClose:OpenOptionsHolder = include(StandardOpenOption.DELETE_ON_CLOSE)
    def deleteOnClose(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.DELETE_ON_CLOSE,inc)

    def dsync:OpenOptionsHolder = include(StandardOpenOption.DSYNC)
    def dsync(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.DSYNC,inc)

    def read:OpenOptionsHolder = include(StandardOpenOption.READ)
    def read(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.READ,inc)

    def sparse:OpenOptionsHolder = include(StandardOpenOption.SPARSE)
    def sparse(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.SPARSE,inc)

    def sync:OpenOptionsHolder = include(StandardOpenOption.SYNC)
    def sync(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.SYNC,inc)

    def truncateExisting:OpenOptionsHolder = include(StandardOpenOption.TRUNCATE_EXISTING)
    def truncateExisting(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.TRUNCATE_EXISTING,inc)

    def write:OpenOptionsHolder = include(StandardOpenOption.WRITE)
    def write(inc:Boolean):OpenOptionsHolder = toggle(StandardOpenOption.WRITE,inc)
  }

  implicit val defaultOptions : OpenOptions = OpenOptionsHolder()
}
