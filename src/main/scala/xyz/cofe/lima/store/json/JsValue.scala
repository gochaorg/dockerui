package xyz.cofe.lima.store.json

import tethys.JsonReader
import tethys.commons.TokenNode
import tethys.readers.ReaderError
import tethys.readers.tokens.{QueueIterator, TokenIterator}

import scala.annotation.tailrec
import scala.collection.immutable

sealed trait JsValue
case object JsNull extends JsValue
case class JsString(string:String) extends JsValue
case class JsBoolean(boolean:Boolean) extends JsValue
case class JsNumber(number:Number) extends JsValue
case class JsArray(items:Seq[JsValue]) extends JsValue
case class JsObject(items:Map[String,JsValue]) extends JsValue

object JsValue {
  implicit val jsonReader:JsonReader[JsValue] = {
    implicitly[JsonReader[InitialReader]].map { initReader =>
      val h = initReader.items.headOption
      if( h.isDefined ){
        h.head
      }else{
        throw new Error("can't fetch from InitialReader")
      }
    }
  }
}

sealed trait Query {
  def fetch(values:List[JsValue]):List[JsValue]
}

object Query {
  case class Field(name:String) extends Query {
    override def fetch(values: List[JsValue]): List[JsValue] = {
      values
        .filter { _.isInstanceOf[JsObject] }
        .map { _.asInstanceOf[JsObject] }
        .flatMap { j =>
          j.items.get(name).map(List(_)).getOrElse(List())
        }
    }
  }
  case class Nested(path:List[Query]) extends Query {
    override def fetch(values: List[JsValue]): List[JsValue] = {
      path.foldLeft(values) { case(res, query) => query.fetch(res) }
    }
  }

  implicit class JsValueOps(val jsRoot:JsValue) {
    def query:QueryBuilder = new QueryBuilder(List(jsRoot))
  }
  implicit class JsValueEthOps[E](val et:Either[E,JsValue]) {
    def query:QueryBuilder = {
      et.map(js => new QueryBuilder(List(js))).getOrElse(
        new QueryBuilder(List())
      )
    }
  }

  class QueryBuilder(val jsRoots:List[JsValue]) {
    def apply(field:String):QueryBuilderExec = QueryBuilderExec(Field(field), jsRoots)
  }

  case class QueryBuilderExec(query:Query, private val jsRoots1:List[JsValue]) extends QueryBuilder(jsRoots1) {
    override def apply(field: String): QueryBuilderExec = {
      copy(
        query =
          query match {
            case fq: Field => Nested(List(fq,Field(field)))
            case Nested(path) => Nested(path :+ Field(field))
          }
      )
    }

    def jsStrings:List[JsString] = query.fetch(jsRoots).filter( j => j.isInstanceOf[JsString] ).map( j => j.asInstanceOf[JsString] )
    def jsString:Option[JsString] = jsStrings.headOption
    def string:Option[String] = jsString.map(_.string)

    def jsObjects:List[JsObject] = query.fetch(jsRoots).filter( j => j.isInstanceOf[JsObject] ).map( j => j.asInstanceOf[JsObject] )
    def jsObject:Option[JsObject] = jsObjects.headOption

    def jsValues:List[JsValue] = query.fetch(jsRoots)
    def jsValue:Option[JsValue] = jsValues.headOption
  }
}

object TethysToks {
  //@tailrec
  def tokens(js:JsValue):List[TokenNode] = {
    js match {
      case JsNull => List(TokenNode.NullValueNode)
      case JsString(string) => List(TokenNode.StringValueNode(string))
      case JsBoolean(boolean) => List(TokenNode.BooleanValueNode(boolean))
      case JsNumber(number) => List(TokenNode.NumberValueNode(number))
      case JsArray(items) =>
        List(TokenNode.ArrayStartNode) ++ items.toList.flatMap(i=>tokens(i)) ++ List(TokenNode.ArrayEndNode)
      case JsObject(items) =>
        List(TokenNode.ObjectStartNode) ++
        items.flatMap { case (name,v) => List(TokenNode.FieldNameNode(name)) ++ tokens(v) } ++
        List(TokenNode.ObjectEndNode)
    }
  }

  implicit class JsonTokens( js:JsValue ) {
    def tokens:List[TokenNode] = TethysToks.tokens(js)
    def tokenIterator:TokenIterator = {
      new QueueIterator(immutable.Queue[TokenNode]().enqueueAll(this.tokens))
    }
    def jsonAs[A:JsonReader]: Either[ReaderError, A] = tokenIterator.readJson[A]
  }
}
