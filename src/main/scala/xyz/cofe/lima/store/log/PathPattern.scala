package xyz.cofe.lima.store.log

import java.nio.file.{Path, Paths}
import scala.collection.mutable

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

  trait Evaluate {
    def eval(code:String):Either[String,String]
  }

  implicit class PatternOps(val pattern:List[Name]) extends AnyVal {
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
        pattern.drop(1).foldLeft(evalFirst(pattern.head)) { case (path,name) =>
          path.flatMap( p => evalNext(p,name) )
        }
      }
    }
  }
}