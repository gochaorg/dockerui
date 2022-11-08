package xyz.cofe.lima.store.config

import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.writers.tokens.TokenWriter
import tethys.{JsonReader, JsonWriter}
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.http.{Duration, SocketReadTimings}
import xyz.cofe.lima.fs.{CopyOptions, JavaNioTracer, LinkOptions, OpenOptions}
import xyz.cofe.lima.store.json._
import xyz.cofe.lima.store.log.PathPattern.PathPattern
import xyz.cofe.lima.store.log.{AppendableFile, AppendableNull, FilesCleaner, PathPattern}

import java.nio.charset.StandardCharsets
import java.nio.file.Path
import xyz.cofe.lima.docker.log.{Logger => DLogger}
import xyz.cofe.lima.launch.InitTasks

/**
 * настройки приложения
 * @param dockerConnect где докер
 */
case class AppConfig
(
  dockerConnect: DockerConnect,
)
object AppConfig {
  implicit val reader:JsonReader[AppConfig] = jsonReader[AppConfig]
  implicit val writer:JsonWriter[AppConfig] = jsonWriter[AppConfig]

  def defaultConfig( unixSocketFile:Path ):AppConfig = {
    import Duration._
    AppConfig(
      dockerConnect =
        DockerConnect.UnixSocketFile(
          fileName = unixSocketFile.toString,
          socketReadTimings = Some(SocketReadTimings(
            cpuThrottling = 1.milliseconds.some,
            readTimeout = 30.seconds.some,
            sourceTimeout = 30.seconds.some
          )),
          dockerLogger = Some(
            DockerLogger.JoinLogger(
              DockerLogger.AllEvent(
                LoggerOutput.FileLog(
                  pathPattern = "{appHome}/log/{yyyy}-{MM}/{dd}/{hh}-{mi}-pid{pid}.dockerClient.stream.json",
                  limitSizePerFile = Some(1024L*512L),
                  FilesCleanup.SummaryMaxSize(1024L*1024L*32L)
                )
              ),
              DockerLogger.FailEvent(
                LoggerOutput.StdErr()
              )
            )
          )
        )
    )
  }
}

/**
 * Как создать коннект
 */
sealed trait DockerConnect
object DockerConnect {
  case class UnixSocketFile
  (
    fileName:String,
    socketReadTimings: Option[SocketReadTimings],
    dockerLogger: Option[DockerLogger],
  ) extends DockerConnect
  object UnixSocketFile {
    implicit val reader: JsonReader[UnixSocketFile] = jsonReader[UnixSocketFile]
    implicit val writer: JsonWriter[UnixSocketFile] = classWriter[UnixSocketFile] ++ jsonWriter[UnixSocketFile]
  }

  implicit val reader:JsonReader[DockerConnect] = JsonReader.builder.addField[String]("_type").selectReader[DockerConnect] {
    case "UnixSocketFile" => UnixSocketFile.reader
  }
  implicit val writer:JsonWriter[DockerConnect] = (value: DockerConnect, tokenWriter: TokenWriter) => {
    value match {
      case u: UnixSocketFile => UnixSocketFile.writer.write(u, tokenWriter)
    }
  }

  implicit class DockerConnectOps(dockerConnect:DockerConnect)(implicit initTasks: InitTasks) {
    def createDockerClient():DockerClient = {
      dockerConnect match {
        case UnixSocketFile(fileName, socketReadTimings, dockerLoggerConf) =>
          val dockerLoggerTask = dockerLoggerConf.flatMap(conf => conf.logger)

          implicit val dockerLoggerInstance: DLogger = dockerLoggerTask.map(_.logger).getOrElse(DLogger.defaultLogger)
          dockerLoggerTask.foreach(_.cleanupTasks.foreach(initTasks.addLogCleanTask))
          DockerClient.unixSocket(fileName, socketReadTimings.getOrElse(new SocketReadTimings()))
      }
    }
  }
}

sealed trait DockerLogger
object DockerLogger {
  /**
   * Нет логгера
   */
  case class NoLogger() extends DockerLogger
  object NoLogger {
    implicit val reader: JsonReader[NoLogger] = jsonReader[NoLogger]
    implicit val writer: JsonWriter[NoLogger] = classWriter[NoLogger] ++ jsonWriter[NoLogger]
  }

  /**
   * Запись всех событий
   * @param target куда писать события
   */
  case class AllEvent(target:LoggerOutput) extends DockerLogger
  object AllEvent {
    implicit val reader: JsonReader[AllEvent] = jsonReader[AllEvent]
    implicit val writer: JsonWriter[AllEvent] = classWriter[AllEvent] ++ jsonWriter[AllEvent]
  }

  /**
   * Запись событий с ошибкой
   * @param target куда писать события
   */
  case class FailEvent(target:LoggerOutput) extends DockerLogger
  object FailEvent {
    implicit val reader: JsonReader[FailEvent] = jsonReader[FailEvent]
    implicit val writer: JsonWriter[FailEvent] = classWriter[FailEvent] ++ jsonWriter[FailEvent]
  }

  /**
   * Запись только успешных событий
   * @param target куда писать события
   */
  case class SuccEvent(target:LoggerOutput) extends DockerLogger
  object SuccEvent {
    implicit val reader: JsonReader[SuccEvent] = jsonReader[SuccEvent]
    implicit val writer: JsonWriter[SuccEvent] = classWriter[SuccEvent] ++ jsonWriter[SuccEvent]
  }

  /**
   * Объединение нескольких логгеров
   * @param left первый логгер
   * @param right второй логгер
   */
  case class JoinLogger(left:DockerLogger, right:DockerLogger) extends DockerLogger
  object JoinLogger {
    implicit val reader: JsonReader[JoinLogger] = jsonReader[JoinLogger]
    implicit val writer: JsonWriter[JoinLogger] = classWriter[JoinLogger] ++ jsonWriter[JoinLogger]
  }

  implicit val writer:JsonWriter[DockerLogger] = (value: DockerLogger, tokenWriter: TokenWriter) => {
    value match {
      case v: NoLogger => NoLogger.writer.write(v, tokenWriter)
      case v: AllEvent => AllEvent.writer.write(v, tokenWriter)
      case v: FailEvent => FailEvent.writer.write(v, tokenWriter)
      case v: SuccEvent => SuccEvent.writer.write(v, tokenWriter)
      case v: JoinLogger => JoinLogger.writer.write(v, tokenWriter)
    }
  }
  implicit val reader:JsonReader[DockerLogger] = JsonReader.builder.addField[String]("_type").selectReader[DockerLogger] {
    case "NoLogger" => NoLogger.reader
    case "AllEvent" => AllEvent.reader
    case "FailEvent" => FailEvent.reader
    case "SuccEvent" => SuccEvent.reader
    case "JoinLogger" => JoinLogger.reader
  }

  case class DockerLoggerTask(logger:DLogger, cleanupTasks:List[Runnable])
  implicit class DockerLoggerOps(dockerLoggerConf:DockerLogger) {
    def logger:Option[DockerLoggerTask] = {
      dockerLoggerConf match {
        case NoLogger() => None
        case AllEvent(target) =>
          val output = target.createOutput
          val cleanup = target.cleanupTask
          val jsonLogger = DLogger.JsonToWriter(output)
          Some( DockerLoggerTask(logger = jsonLogger, cleanupTasks = if(cleanup.isEmpty)List()else List(cleanup.get)) )
        case FailEvent(target) =>
          val output = target.createOutput
          val cleanup = target.cleanupTask
          val jsonLogger = DLogger.failLogger(DLogger.JsonToWriter(output))
          Some( DockerLoggerTask(logger = jsonLogger, cleanupTasks = if(cleanup.isEmpty)List()else List(cleanup.get)) )
        case SuccEvent(target) =>
          val output = target.createOutput
          val cleanup = target.cleanupTask
          val jsonLogger = DLogger.succLogger(DLogger.JsonToWriter(output))
          Some( DockerLoggerTask(logger = jsonLogger, cleanupTasks = if(cleanup.isEmpty)List()else List(cleanup.get)) )
        case JoinLogger(left, right) =>
          val leftTask = left.logger
          val rightTask = right.logger
          leftTask.zip(rightTask).map { case(lt, rt) =>
            DockerLoggerTask(DLogger.joinLoggers(lt.logger, rt.logger), lt.cleanupTasks ++ rt.cleanupTasks)
          }.orElse(leftTask).orElse(rightTask)
      }
    }
  }
}

/**
 * Как/куда писать лог
 */
sealed trait LoggerOutput
object LoggerOutput {
  case class NoLogger() extends LoggerOutput
  object NoLogger {
    implicit val reader:JsonReader[NoLogger] = jsonReader[NoLogger]
    implicit val writer:JsonWriter[NoLogger] = classWriter[NoLogger] ++ jsonWriter[NoLogger]
  }

  case class StdOut() extends LoggerOutput
  object StdOut {
    implicit val reader:JsonReader[StdOut] = jsonReader[StdOut]
    implicit val writer:JsonWriter[StdOut] = classWriter[StdOut] ++ jsonWriter[StdOut]
  }

  case class StdErr() extends LoggerOutput
  object StdErr {
    implicit val reader:JsonReader[StdErr] = jsonReader[StdErr]
    implicit val writer:JsonWriter[StdErr] = classWriter[StdErr] ++ jsonWriter[StdErr]
  }

  case class FileLog(pathPattern:String, limitSizePerFile:Option[Long], cleanup:FilesCleanup) extends LoggerOutput
  object FileLog {
    implicit val reader:JsonReader[FileLog] = jsonReader[FileLog]
    implicit val writer:JsonWriter[FileLog] = classWriter[FileLog] ++ jsonWriter[FileLog]
  }

  implicit val writer:JsonWriter[LoggerOutput] = (value: LoggerOutput, tokenWriter: TokenWriter) => {
    value match {
      case u: NoLogger => NoLogger.writer.write(u, tokenWriter)
      case u: StdOut => StdOut.writer.write(u, tokenWriter)
      case u: StdErr => StdErr.writer.write(u, tokenWriter)
      case u: FileLog => FileLog.writer.write(u, tokenWriter)
    }
  }
  implicit val reader:JsonReader[LoggerOutput] = JsonReader.builder.addField[String]("_type").selectReader[LoggerOutput] {
    case "NoLogger" => NoLogger.reader
    case "StdOut" => StdOut.reader
    case "StdErr" => StdErr.reader
    case "FileLog" => FileLog.reader
  }

  implicit class LoggerOutputOps(val loggerOutput: LoggerOutput)(implicit
                                                                 openOptions: OpenOptions,
                                                                 linkOptions: LinkOptions,
                                                                 copyOptions: CopyOptions,
                                                                 trace: JavaNioTracer) {
    def createOutput: Appendable = loggerOutput match {
      case NoLogger() => new AppendableNull()
      case StdOut() => System.out
      case StdErr() => System.err
      case FileLog(pathPattern, limitSizePerFile, cleanup) =>
        val pattern = PathPattern.parse(Path.of(pathPattern))
        val tryGen = pattern.generate
        if( tryGen.isLeft )throw new Error(s"invalid pattern $pattern: ${tryGen.left.getOrElse("???")}")
        AppendableFile(pattern, StandardCharsets.UTF_8, limitSizePerFile)
    }

    def cleanupTask:Option[Runnable] = loggerOutput match {
      case NoLogger() => None
      case StdOut() => None
      case StdErr() => None
      case FileLog(pathPattern, limitSizePerFile, cleanup) => cleanup.cleanupTask(PathPattern.parse(Path.of(pathPattern)))
    }
  }
}

/**
 * Как чистить логи
 */
sealed trait FilesCleanup
object FilesCleanup {
  /**
   * не удалять файлы
   */
  case class NoCleanup() extends FilesCleanup
  object NoCleanup {
    implicit val reader:JsonReader[NoCleanup] = jsonReader[NoCleanup]
    implicit val writer:JsonWriter[NoCleanup] = classWriter[NoCleanup] ++ jsonWriter[NoCleanup]
  }

  /**
   * удалять файлы, если сумарный размер превышает указанный
   * @param maxSize максимально допустимый размер
   */
  case class SummaryMaxSize(maxSize:Long) extends FilesCleanup
  object SummaryMaxSize {
    implicit val reader:JsonReader[SummaryMaxSize] = jsonReader[SummaryMaxSize]
    implicit val writer:JsonWriter[SummaryMaxSize] = classWriter[SummaryMaxSize] ++ jsonWriter[SummaryMaxSize]
  }

  implicit val writer:JsonWriter[FilesCleanup] = (value: FilesCleanup, tokenWriter: TokenWriter) => {
    value match {
      case u: NoCleanup => NoCleanup.writer.write(u, tokenWriter)
      case u: SummaryMaxSize => SummaryMaxSize.writer.write(u, tokenWriter)
    }
  }
  implicit val reader:JsonReader[FilesCleanup] = JsonReader.builder.addField[String]("_type").selectReader[FilesCleanup] {
    case "NoCleanup" => NoCleanup.reader
    case "SummaryMaxSize" => SummaryMaxSize.reader
  }

  /**
   * Создание задач обслуживания логов
   * @param filesCleanup как чистить логи
   * @param openOptions опции фс
   * @param linkOptions опции фс
   * @param copyOptions опции фс
   * @param trace трессировка фс
   */
  implicit class FilesCleanupOps( val filesCleanup: FilesCleanup )(implicit
                                                                   openOptions: OpenOptions,
                                                                   linkOptions: LinkOptions,
                                                                   copyOptions: CopyOptions,
                                                                   trace: JavaNioTracer){
    def cleanupTask(pathPattern: PathPattern):Option[Runnable] = {
      filesCleanup match {
        case NoCleanup() => None
        case SummaryMaxSize(maxSize) => Some(()=>{
          FilesCleaner.clean(pathPattern, maxSize)
        })
      }
    }
  }
}
