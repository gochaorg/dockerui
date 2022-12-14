package xyz.cofe.lima.store

import java.nio.file.Path
import xyz.cofe.lima.fs.syntax._
import xyz.cofe.lima.store.config.AppConfig

/**
 * Каталог с настройками приложения
 */
object AppHome {
  def setSystemParam(path: Path):Unit = {
    System.getProperties.setProperty("dockerui.appHome", path.toString)
  }
  private lazy val bySystemParam:Option[Path] = {
    System.getProperties.getProperty("dockerui.appHome") match {
      case value:String => Some(Path.of(value))
      case _ => None
    }
  }

  private lazy val byEnvParam:Option[Path] = {
    System.getenv("DOCKERUI_HOME") match {
      case value: String => Some(Path.of(value))
      case _ => None
    }
  }

  private val dirName = ".dockerui"

  private lazy val byLocalDir: Option[Path] = {
    Path.of(".").canonical.upPath.reverse.foldLeft(None:Option[Path]) {
      case (res,pth) =>
        res match {
          case Some(value) => res
          case None => pth.resolve(dirName).isDir match {
            case Left(err) => res
            case Right(false) => res
            case Right(true) => Some(pth.resolve(dirName))
          }
        }
    }
  }

  private lazy val byHomeDir: Option[Path] = {
    val dir = Path.of(System.getProperty("user.dir")).resolve(dirName)
    dir.isDir match {
      case Right(true) => Some(dir)
      case _ => None
    }
  }

  private lazy val defaultDir:Path = Path.of(".").resolve(dirName)

  /**
   * Каталог с настройками приложения
   */
  lazy val directory:Path =
    bySystemParam.getOrElse(
      byEnvParam.getOrElse(
        byLocalDir.getOrElse(
          byHomeDir.getOrElse(
            defaultDir
          )
        )
      )
    )

  lazy val appConfigPath:Path = directory.resolve("app-config.json")

  def readConfig:Either[Throwable,AppConfig] =
    appConfigPath.json.read[AppConfig]

  def writeConfig(config:AppConfig): Either[Throwable, Unit] = {
    for {
      prnt <- appConfigPath.parent.toRight("parentNull")
      dir <- prnt.isDir
      _ <- if( !dir ){ prnt.createDirectories } else Right()
    } yield ()
    appConfigPath.json.write(config,pretty = true)
  }
}
