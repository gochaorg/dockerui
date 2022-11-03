package xyz.cofe.lima.store.json

import tethys.JsonReader
import tethys.commons.Token
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator

sealed trait JsonTreeReader {
  def parent: Option[JsonTreeReader]

  def read(it: TokenIterator): JsonTreeReader

  def accept(objectReader: ObjectReader): JsonTreeReader = {
    parent.map(p => p.accept(objectReader)).getOrElse(this)
  }

  def accept(arrayReader: ArrayReader): JsonTreeReader = {
    parent.map(p => p.accept(arrayReader)).getOrElse(this)
  }

  def accept(fieldReader: FieldReader, value: JsValue): JsonTreeReader = {
    parent.map(p => p.accept(fieldReader, value)).getOrElse(this)
  }

  def path: List[JsonTreeReader] = {
    var pth = List[JsonTreeReader]()
    var p: Option[JsonTreeReader] = Some(this)
    while (p.isDefined) {
      pth = p.get :: pth
      p = p.get.parent
    }
    pth
  }
}

object JsonTreeReader {
  implicit def read: JsonReader[InitialReader] = new JsonReader[InitialReader] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): InitialReader = {
      var state: JsonTreeReader = new InitialReader()
      var stop = false
      while (!stop) {
        state match {
          case s: InitialReader =>
            if (s.items.nonEmpty) {
              stop = true
            } else {
              state = state.read(it)
              it.next()
            }
          case s: FailStateReader =>
            stop = true
          case _ =>
            state = state.read(it)
            it.next()
        }
      }

      state match {
        case s: InitialReader => s
        case _ => throw new Error(s"fail read: ${state.path}")
      }
    }
  }
}

class InitialReader extends JsonTreeReader {
  var items: List[JsValue] = List()

  override def parent: Option[JsonTreeReader] = None

  override def read(it: TokenIterator): JsonTreeReader = {
    it.currentToken() match {
      case Token.ArrayStartToken => new ArrayReader(Some(this))
      case Token.ObjectStartToken => new ObjectReader(Some(this))

      case Token.StringValueToken =>
        items = items :+ JsString(it.string())
        this
      case Token.NumberValueToken =>
        items = items :+ JsNumber(it.number())
        this
      case Token.BooleanValueToken =>
        items = items :+ JsBoolean(it.boolean())
        this
      case Token.NullValueToken =>
        items = items :+ JsNull
        this

      case Token.ArrayEndToken
           | Token.FieldNameToken
           | Token.ObjectEndToken
           | Token.Empty => new FailStateReader(Some(this), it.currentToken())
    }
  }

  override def accept(objectReader: ObjectReader): JsonTreeReader = {
    items = items :+ objectReader.toJsObject
    this
  }

  override def accept(arrayReader: ArrayReader): JsonTreeReader = {
    items = items :+ arrayReader.toJsArray
    this
  }

  override def toString = s"InitialReader()"
}

class ObjectReader(val parent: Option[JsonTreeReader]) extends JsonTreeReader {
  var fields: Map[String, JsValue] = Map()

  override def read(it: TokenIterator): JsonTreeReader = {
    it.currentToken() match {
      case Token.ObjectEndToken =>
        accept(this)
      case Token.FieldNameToken => new FieldReader(Some(this), it.fieldName())
      //
      case Token.StringValueToken
           | Token.NumberValueToken
           | Token.BooleanValueToken
           | Token.NullValueToken
           | Token.ArrayStartToken
           | Token.ArrayEndToken
           | Token.ObjectStartToken
           | Token.Empty => new FailStateReader(Some(this), it.currentToken())
    }
  }

  override def accept(fieldReader: FieldReader, value: JsValue): JsonTreeReader = {
    fields = fields + (fieldReader.fieldName -> value)
    this
  }

  override def toString = s"ObjectReader()"

  def toJsObject:JsObject = JsObject(fields)
}

class FieldReader(val parent: Option[JsonTreeReader], val fieldName: String) extends JsonTreeReader {
  override def read(it: TokenIterator): JsonTreeReader = {
    it.currentToken() match {
      case Token.StringValueToken =>
        accept(this, JsString(it.string()))
      case Token.NumberValueToken =>
        accept(this, JsNumber(it.number()))
      case Token.BooleanValueToken =>
        accept(this, JsBoolean(it.boolean()))
      case Token.NullValueToken =>
        accept(this, JsNull)
      case Token.ArrayStartToken => new ArrayReader(Some(this))
      case Token.ObjectStartToken => new ObjectReader(Some(this))
      case Token.FieldNameToken | Token.ArrayEndToken | Token.ObjectEndToken | Token.Empty => new FailStateReader(Some(this), it.currentToken())
    }
  }

  override def accept(objectReader: ObjectReader): JsonTreeReader = {
    accept(this, objectReader.toJsObject)
  }

  override def accept(arrayReader: ArrayReader): JsonTreeReader = {
    accept(this, arrayReader.toJsArray)
  }

  override def toString = s"FieldReader(${fieldName})"
}

class ArrayReader(val parent: Option[JsonTreeReader]) extends JsonTreeReader {
  var items: List[JsValue] = List()

  override def read(it: TokenIterator): JsonTreeReader = {
    it.currentToken() match {
      case Token.StringValueToken =>
        items = items :+ JsString(it.string())
        this
      case Token.NumberValueToken =>
        items = items :+ JsNumber(it.number())
        this
      case Token.BooleanValueToken =>
        items = items :+ JsBoolean(it.boolean())
        this
      case Token.NullValueToken =>
        items = items :+ JsNull
        this
      case Token.ArrayStartToken => new ArrayReader(Some(this))
      case Token.ArrayEndToken =>
        super.accept(this)
      case Token.ObjectStartToken => new ObjectReader(Some(this))
      case Token.ObjectEndToken | Token.FieldNameToken | Token.Empty => new FailStateReader(Some(this), it.currentToken())
    }
  }

  override def accept(objectReader: ObjectReader): JsonTreeReader = {
    items = items :+ objectReader.toJsObject
    this
  }

  override def accept(arrayReader: ArrayReader): JsonTreeReader = {
    items = items :+ arrayReader.toJsArray
    this
  }

  override def toString = s"ArrayReader()"

  def toJsArray:JsArray = JsArray(items)
}

class FailStateReader(val parent: Option[JsonTreeReader], val token: Token) extends JsonTreeReader {
  override def read(it: TokenIterator): JsonTreeReader = {
    this
  }

  override def toString = s"FailStateReader()"
}

