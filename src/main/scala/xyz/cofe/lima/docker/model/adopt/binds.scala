package xyz.cofe.lima.docker.model
package adopt

/**
 * Адоптация монтирования дисков
 */
object binds {
  // A list of volume bindings for this container. Each volume binding is a string in one of these forms:
  //
  //host-src:container-dest[:options] to bind-mount a host path into the container. Both host-src, and container-dest must be an absolute path.
  //volume-name:container-dest[:options] to bind-mount a volume managed by a volume driver into the container. container-dest must be an absolute path.
  //options is an optional, comma-delimited list of:
  //
  //nocopy disables automatic copying of data from the container path to the volume. The nocopy flag only applies to named volumes.
  //[ro|rw] mounts a volume read-only or read-write, respectively. If omitted or set to rw, volumes are mounted read-write.
  //[z|Z] applies SELinux labels to allow or deny multiple containers to read and write to the same volume.
  //z: a shared content label is applied to the content. This label indicates that multiple containers can share the volume content, for both reading and writing.
  //Z: a private unshared label is applied to the content. This label indicates that only the current container can use a private volume. Labeling systems such as SELinux require proper labels to be placed on volume content that is mounted into a container. Without a label, the security system can prevent a container's processes from using the content. By default, the labels set by the host operating system are not modified.
  //[[r]shared|[r]slave|[r]private] specifies mount propagation behavior. This only applies to bind-mounted volumes, not internal volumes or named volumes. Mount propagation requires the source mount point (the location where the source directory is mounted in the host operating system) to have the correct propagation properties. For shared volumes, the source mount point must be set to shared. For slave volumes, the mount must be set to either shared or slave.

  sealed trait RWOption
  case object ReadOnly extends RWOption
  case object ReadWrite extends RWOption

  sealed trait Propagation
  case object Shared extends Propagation
  case object Slave extends Propagation
  case object Private extends Propagation
  case object RShared extends Propagation
  case object RSlave extends Propagation
  case object RPrivate extends Propagation

  sealed trait SELabels
  case object SEShared extends SELabels
  case object SEPrivate extends SELabels

  case class DirBind(source:String,
                     destination:String,
                     unparsedOptions:List[String] = List(),
                     nocopy:Boolean = false,
                     rwOption:Option[RWOption] = None,
                     seLabels:Option[SELabels] = None,
                     propagation:Option[Propagation] = None,
                    ) {
    import scala.collection.mutable

    lazy val line:String = {
      val sb = new mutable.StringBuilder()
      sb ++= source
      sb ++= ":"
      sb ++= destination

      val opt = List(
        if(nocopy) List("nocopy") else List(),
        rwOption.map {
          case ReadOnly => List("ro")
          case ReadWrite => List("rw")
        }.getOrElse(List()),
        seLabels.map {
          case SEShared => List("z")
          case SEPrivate => List("Z")
        }.getOrElse(List()),
        propagation.map {
          case Shared => List("shared")
          case Slave => List("slave")
          case Private => List("private")
          case RShared => List("rshared")
          case RSlave => List("rslave")
          case RPrivate => List("rprivate")
        }.getOrElse(List()),
        unparsedOptions
      ).flatten.mkString(",")
      if(opt.nonEmpty)sb.append(":").append(opt)

      sb.toString()
    }
  }

  /**
   * Парсинг строки указыающий bind каталогов
   * @param line
   * Формат:
   *
   * <pre>
   * line ::= host-src ':' container-dest [ ':' options ]
   *        | volume-name ':' container-dest [ ':' options ]
   *
   * host-src - must be an absolute path
   * container-dest - must be an absolute path
   *
   * options ::= option { ',' option }
   * option ::= 'nocopy'
   *          | 'ro'
   *          | 'rw'
   *          | 'z'
   *          | 'Z'
   *          | [ 'r' ] [ 'shared' | 'slave' | 'private' ]
   * </pre>
   *
   * options
   *
   *  - '''nocopy''' disables automatic copying of data from the container path to the volume.
   *                 flag only applies to named volumes
   *
   *  - '''ro''' - mounts a volume read-only
   *  - '''rw''' - ('''default''') mounts a volume read-write
   *  - '''z''' - a shared content label is applied to the content. This label indicates that multiple containers
   *   can share the volume content, for both reading and writing
   *  - '''Z''' - a private unshared label is applied to the content.
   *   This label indicates that only the current container can use a private volume.
   *   Labeling systems such as SELinux require proper labels to be placed on volume
   *   content that is mounted into a container.
   *   Without a label, the security system can prevent a container's
   *   processes from using the content. By default, the labels set by the host operating system are not modified.
   *  - '''r shared slave private''' -
   *   specifies mount propagation behavior.
   *   This only applies to bind-mounted volumes, not internal volumes or named volumes.
   *   Mount propagation requires the source mount point (the location where the source directory
   *   is mounted in the host operating system) to have the correct propagation properties.
   *   For shared volumes, the source mount point must be set to shared.
   *   For slave volumes, the mount must be set to either shared or slave.
   *
   * @return Результат парсинга
   */
  def parse(line:String):Either[String,DirBind] = {
    val values = line.split(":",3)
    if( values.length==2 ){
      Right(DirBind(values(0),values(1)))
    }else if( values.length==3 ){
      val optString = values(2)
      val opts = optString.split(",").toList
      Right {
        opts.foldLeft(DirBind(values(0), values(1))) {
          case (bind, opt) =>
            opt match {
              case "nocopy" => bind.copy(nocopy = true)
              case "ro" => bind.copy(rwOption = Some(ReadOnly))
              case "rw" => bind.copy(rwOption = Some(ReadWrite))
              case "z" => bind.copy(seLabels = Some(SEShared))
              case "Z" => bind.copy(seLabels = Some(SEPrivate))
              case "shared" => bind.copy(propagation = Some(Shared))
              case "slave" => bind.copy(propagation = Some(Slave))
              case "private" => bind.copy(propagation = Some(Private))
              case "rshared" => bind.copy(propagation = Some(RShared))
              case "rslave" => bind.copy(propagation = Some(RSlave))
              case "rprivate" => bind.copy(propagation = Some(RPrivate))
              case _ =>
                bind.copy(unparsedOptions = opt :: bind.unparsedOptions)
            }
        }
      }
    }else {
      Left("expect : delimiter")
    }
  }

  implicit class HostConfigOps(hostConfig:HostConfig) {
    def readEither:Either[String,List[DirBind]] =
      hostConfig.Binds
        .map(_.map(parse))
        .getOrElse(List())
        .foldLeft(Right(List.empty):Either[String,List[DirBind]] ){ case (sum,dirBindEt) =>
          sum.flatMap(lst => dirBindEt.map( dirBind => lst :+ dirBind ))
      }

    def read:List[DirBind] = readEither match {
      case Left(value) => throw new Error(s"not parsed! $value")
      case Right(value) => value
    }

    def write(list:List[DirBind]):HostConfig = {
      hostConfig.copy(
        Binds = Option.when(list.nonEmpty){
          list.map(_.line)
        }
      )
    }
  }
}
