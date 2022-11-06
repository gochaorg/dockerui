package xyz.cofe.lima.store.log

import xyz.cofe.lima.store.AppHome

import java.nio.file.Path
import java.time._
import java.util.Locale
import java.util.regex.Pattern
import scala.collection.mutable

/**
 * шаблон имени генерируемого файла
 */
object PathPattern {
  sealed trait Name
  object Name {
    case object AbsolutePrefix extends Name
    case class Plain(text:String) extends Name
    case class Template(parts:List[TemplatePart]) extends Name
  }

  sealed trait TemplatePart
  object TemplatePart {
    case class Plain(text:String) extends TemplatePart
    case class Code(code:String) extends TemplatePart
  }

  def parseTemplate(name:String):Name.Template = {
    var result = List[TemplatePart]()
    var state = 0
    var level = 0
    val sb = new mutable.StringBuilder()
    (0 until name.length).foreach { ci =>
      val ch = name(ci)
      state match {
        case 0 =>
          ch match {
            case '{' => state = 1
              if( sb.nonEmpty ){
                result = TemplatePart.Plain(sb.toString()) :: result
                sb.clear()
              }
              level = 1
            case '\\' => state = 2
            case _ => sb.append(ch)
          }
        case 1 =>
          ch match {
            case '{' =>
              sb.append(ch)
              level += 1
            case '}' =>
              level -= 1
              if( level==0 && sb.nonEmpty ){
                result = TemplatePart.Code(sb.toString()) :: result
                sb.clear()
                state = 0
              }else{
                sb.append(ch)
              }
            case _ =>
              sb.append(ch)
          }
        case 2 =>
          ch match {
            case 'n' => sb.append("\n")
            case 'r' => sb.append("\r")
            case 't' => sb.append("\t")
            case _ => sb.append(ch)
          }
        case _ =>
      }
    }

    if( sb.nonEmpty ){
      state match {
        case 0 => result = TemplatePart.Plain(sb.toString()) :: result
        case 1 => result = TemplatePart.Code(sb.toString()) :: result
      }
    }

    Name.Template(result.reverse)
  }
  def parse(path: Path):List[Name] = {
    (0 until path.getNameCount).foldLeft(
      if (path.isAbsolute) {
        List[Name](Name.AbsolutePrefix)
      } else {
        List[Name]()
      }
    ) { case(ptrn,idx) =>
      val name = path.getName(idx).toString
      (
        parseTemplate(name) match {
          case Name.Template(List(TemplatePart.Plain(plain))) => Name.Plain(plain)
          case r => r
        }
      ) :: ptrn
    }.reverse
  }
  def escape(path: Path):List[Name] = {
    (0 until path.getNameCount).foldLeft(
      if (path.isAbsolute) {
        List[Name](Name.AbsolutePrefix)
      } else {
        List[Name]()
      }
    ) { case (ptrn, idx) =>
      val name = path.getName(idx).toString
      Name.Plain(name) :: ptrn
    }.reverse
  }

  trait Evaluate {
    def eval(code:String):Either[String,String]
    def namePattern(code:String):Either[String,String]
    def pathPattern(code:String):Either[String,Path] = Left(s"undefined $code")
    def cached:Evaluate = this match {
      case EvaluateCache(_) => this
      case _ => EvaluateCache(this)
    }
  }
  case class EvaluateCache(source:Evaluate) extends Evaluate {
    private var cache: Map[String,Either[String,String]] = Map()
    override def eval(code: String): Either[String, String] = {
      cache.get(code) match {
        case Some(value) => value
        case None =>
          val res = source.eval(code)
          cache = cache + (code -> res)
          res
      }
    }
    override def namePattern(code: String): Either[String, String] = {
      source.namePattern(code)
    }

    override def pathPattern(code: String): Either[String, Path] = {
      source.pathPattern(code)
    }
  }

  object PatternOps {
    lazy val nonQuoted: Pattern = Pattern.compile("[\\d\\w\\-_]*")
    def quote(str:String):String = {
      if( PatternOps.nonQuoted.matcher(str).matches() ){
        str
      } else {
        Pattern.quote(str)
      }
    }
  }

  case class PathPredicate(validator:Path=>Boolean, index:Option[Int]=None)

  sealed trait Evaluated
  case class EvlPlain(string:String) extends Evaluated
  case class EvlRegex(string:String) extends Evaluated
  case class EvlFail(string:String) extends Evaluated
  case class EvlPath(path:Path) extends Evaluated

  implicit class PatternOps(val pattern:List[Name]) extends AnyVal {
    import PatternOps._

    private def evalFirst(name:Name)(implicit evaluate: Evaluate):Either[String,Path] = {
      name match {
        case Name.AbsolutePrefix => Right(Path.of("/"))
        case Name.Plain(text) => Right(Path.of(text))
        case Name.Template(parts) =>
          parts.map {
            case TemplatePart.Plain(text) => Right(text)
            case TemplatePart.Code(code) => evaluate.eval(code)
          }.foldLeft( Right(""):Either[String,String] ){ case(str, pstr) =>
            str match {
              case Left(value) => str
              case Right(leftStr) => pstr.map { rightStr =>
                leftStr + rightStr
              }
            }
          }.map( str => Path.of(str))
      }
    }
    private def evalNext(path:Path,name:Name)(implicit evaluate: Evaluate):Either[String,Path] = {
      name match {
        case Name.AbsolutePrefix => Right(path)
        case Name.Plain(text) => Right(path.resolve(text))
        case Name.Template(parts) =>
          parts.map {
            case TemplatePart.Plain(text) => Right(text)
            case TemplatePart.Code(code) => evaluate.eval(code)
          }.foldLeft(Right(""): Either[String, String]) { case (str, pstr) =>
            str match {
              case Left(value) => str
              case Right(leftStr) => pstr.map { rightStr =>
                leftStr + rightStr
              }
            }
          }.map(str => path.resolve(str))
      }
    }
    def generate(implicit evaluate: Evaluate):Either[String,Path] = {
      if( pattern.isEmpty ){
        Left("empty")
      } else {
        val ev = evaluate.cached
        pattern.drop(1).foldLeft(evalFirst(pattern.head)(ev)) { case (path,name) =>
          path.flatMap( p => evalNext(p,name)(ev) )
        }
      }
    }

    private def predNameEquals(index:Int, sample:String):PathPredicate = {
      PathPredicate(index=Some(index), validator = pth => {
        if( index<pth.getNameCount ){
          val name = pth.getName(index).toString
          sample == name
        }else{
          false
        }
      })
    }
    private def predNameRegex(index:Int, regex:String):PathPredicate = {
      val ptrn = Pattern.compile(regex)
      PathPredicate(index = Some(index), validator = pth => {
        if (index < pth.getNameCount) {
          val name = pth.getName(index).toString
          ptrn.matcher(name).matches()
        } else {
          false
        }
      })
    }

    private def pred(name:Name, before:List[PathPredicate]=List() )(implicit evaluate: Evaluate):Either[String,List[PathPredicate]] = {
      name match {
        case Name.AbsolutePrefix =>
          if( before.isEmpty ) {
            Right(List(PathPredicate(path => path.isAbsolute)))
          }else{
            Right(before)
          }
        case Name.Plain(text) =>
          val idx = before.lastOption.flatMap(_.index).getOrElse(-1)+1
          Right( before ++ List(predNameEquals(idx, text)))
        case Name.Template(parts) =>
          parts.map {
            case TemplatePart.Plain(text) =>
              EvlPlain(text)
            case TemplatePart.Code(code) =>
              evaluate.namePattern(code) match {
              case Right(value) =>
                EvlRegex(value)
              case Left(err1) => evaluate.pathPattern(code) match {
                case Left(err2) =>
                  EvlFail(err2)
                case Right(value) =>
                  EvlPath(value)
              }
            }
          }.foldLeft( Right(before ):Either[String,List[PathPredicate]] ){ case (sum, evl) =>
            sum.flatMap { lstPred =>
              evl match {
                case EvlFail(string) => Left(string)
                case EvlPlain(string) =>
                  Right( lstPred :+ predNameEquals(lstPred.lastOption.flatMap(_.index).getOrElse(-1)+1, string) )
                case EvlRegex(string) =>
                  Right( lstPred :+ predNameRegex(lstPred.lastOption.flatMap(_.index).getOrElse(-1)+1, string) )
                case EvlPath(path) =>
                  val firstIdx = lstPred.lastOption.flatMap(_.index).getOrElse(-1)+1
                  val prefixLst = if( lstPred.isEmpty && path.isAbsolute ){
                    List(PathPredicate(path=>path.isAbsolute))
                  }else{
                    List()
                  }

                  Right(lstPred ++
                  prefixLst ++
                  ((0 until(path.getNameCount))
                    .map(path.getName)
                    .map(_.toString)
                    .zipWithIndex.map { case (name,idx) => predNameEquals(idx+firstIdx,name) }
                    .toList))
              }
            }
          }
      }
    }
//    private def predNext(before:List[PathPredicate], name:Name)(implicit evaluate: Evaluate):Either[String,List[PathPredicate]] = {
//      val idx = before.head.index.getOrElse(-1) + 1
//      name match {
//        case Name.AbsolutePrefix => Right(before)
//        case Name.Plain(text) => Right {
//          predNameEquals(idx,text) :: before
//        }
//        case Name.Template(parts) =>
//          parts.map {
//            case TemplatePart.Plain(text) => Right(quote(text))
//            case TemplatePart.Code(code) => evaluate.namePattern(code)
//          }.foldLeft(Right(""): Either[String, String]) { case (str, ptrn) => str match {
//            case Left(_) => str
//            case Right(leftStr) => ptrn.map { rightStr => leftStr + rightStr }
//          }
//          }.map { regex =>
//            predNameRegex(idx, regex) :: before
//          }
//      }
//    }

    def predicate(implicit evaluate: Evaluate):Either[String,Path=>Boolean] = {
      if( pattern.isEmpty ){
        Left("empty")
      }else{
        val ev = evaluate.cached
//        pattern.drop(1).foldLeft(
//          pred(pattern.head)(ev)
//        ) { case (ptrn,name) =>
//          ptrn.flatMap { ptrns =>
//            pred( ptrns, name )
//          }
//        }.map { pathPredicates =>
//          path => {
//            pathPredicates.map { pathPred => pathPred.validator(path) }.forall( x => x )
//          }
//        }

        pattern.foldLeft( Right(List()):Either[String,List[PathPredicate]] ){ case(sum, name) =>
          sum.flatMap { predList =>
            pred(name,predList)(ev)
          }
        }.map { predList =>
          path => {
            predList.map { pred => pred.validator(path) }
          }.forall(x=>x)
        }
      }
    }
  }

  implicit class StrOps(val str:String) extends AnyVal {
    def alignLeft(len: Int, chr: Char): String = {
      if (str.length >= len) {
        str
      } else {
        str + s"$chr" * (len - str.length)
      }
    }
    def alignRight(len: Int, chr: Char): String = {
      if (str.length >= len) {
        str
      } else {
        s"$chr" * (len - str.length) + str
      }
    }
  }

  trait AppHomeProvider {
    def appHome:Path
  }
  object AppHomeProvider {
    implicit val defaultInstance: AppHomeProvider = new AppHomeProvider {
      override def appHome: Path = AppHome.directory
    }
    def provide(home:Path):AppHomeProvider = new AppHomeProvider {
      override def appHome: Path = home
    }
  }

  object Evaluate {
    case class Time(time:Instant = Instant.now()) {
      lazy val localTime: LocalDateTime = time.atZone(ZoneId.systemDefault()).toLocalDateTime
      lazy val year: Int = localTime.getYear
      lazy val month: Month = localTime.getMonth
      lazy val dayOfMonth: Int = localTime.getDayOfMonth
      lazy val dayOfWeek: DayOfWeek = localTime.getDayOfWeek
      lazy val hour: Int = localTime.getHour
      lazy val minute: Int = localTime.getMinute
      lazy val second: Int = localTime.getSecond
    }

    implicit def defaultEvaluate(implicit appHomeProvider: AppHomeProvider): Evaluate = new Evaluate {
      lazy val time: Time = Time()
      lazy val pid: Long = {
        ProcessHandle.current().pid()
      }
      lazy val appHome: Path = appHomeProvider.appHome

      override def eval(code: String): Either[String, String] = {
        code match {
          case "appHome" => Right(appHome.toString)
          case "pid" => Right(pid.toString)
          case "yy" | "YY" => Right(time.year.toString.alignRight(4,'0').substring(2))
          case "yyyy" | "YYYY" => Right(time.year.toString.alignRight(4,'0'))
          case "MM" | "Mm" => Right(time.month.getValue.toString.alignRight(2,'0'))
          case "MMM" | "Mmm" => Right(time.month.getDisplayName(java.time.format.TextStyle.SHORT,Locale.US))
          case "dd" => Right(time.dayOfMonth.toString.alignRight(2,'0'))
          case "hh" | "HH" => Right(time.hour.toString.alignRight(2,'0'))
          case "mm" | "mi" => Right(time.minute.toString.alignRight(2,'0'))
          case "ss" | "SS" => Right(time.second.toString.alignRight(2,'0'))
          case _ => Left(s"undefined $code")
        }
      }
      override def namePattern(code: String): Either[String, String] = {
        code match {
          case "pid" => Right("\\d+")
          case "yy" | "YY" => Right("\\d{2}")
          case "yyyy" | "YYYY" => Right("\\d{4}")
          case "MM" | "Mm" => Right("\\d{2}")
          case "MMM" | "Mmm" => Right("\\w{3}")
          case "dd" | "hh" | "HH" | "mm" | "mi" | "ss" | "SS" => Right("\\d{2}")
          case _ => Left(s"undefined $code")
        }
      }

      override def pathPattern(code: String): Either[String, Path] = {
        code match {
          case "appHome" => Right(appHome)
          case _ => Left(s"undefined $code")
        }
      }
    }
  }
}