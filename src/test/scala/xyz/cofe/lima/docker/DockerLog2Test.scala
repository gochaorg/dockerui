package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import tethys.JsonReader
import tethys.commons.Token
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator
import tethys._
import tethys.jackson._

import java.util.concurrent.atomic.AtomicInteger

class DockerLog2Test extends AnyFunSuite {
  val logEntry =
    """{"_type":"SuccEvent",
      |  "threadId":{"id":18,"name":"JavaFX Application Thread"},
      |  "beginTime":"2022-10-31T04:01:21.057027419",
      |  "endTime":"2022-10-31T04:01:21.141861409",
      |  "args":{
      |    "_type":"Containers","all":true,"size":false
      |  },
      |
      |  "result":[
      |    {
      |      "Id":"53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4",
      |      "Names":["/n1"],
      |      "Image":"alpine",
      |      "ImageID":"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5",
      |      "Command":"echo test",
      |      "Created":1666480250,
      |      "Ports":[],
      |      "Labels":{},
      |      "State":"exited","Status":"Exited (0) 8 days ago",
      |      "HostConfig":{"NetworkMode":"default"},
      |      "NetworkSettings":
      |      {
      |        "Networks":{
      |          "bridge":{
      |            "NetworkID":"89e112550a285c17d70fea06881feb46212aa788180181344e0d95158b88b10f",
      |            "EndpointID":"",
      |            "Gateway":"",
      |            "IPAddress":"",
      |            "IPPrefixLen":0,
      |            "IPv6Gateway":"",
      |            "GlobalIPv6Address":"",
      |            "GlobalIPv6PrefixLen":0,
      |            "MacAddress":""
      |           }
      |         }
      |       }
      |     },
      |     {
      |       "Id":"1668f9b53221b7cc09beb97c162a23ead66d11a1fd0127bcb10228869d55da79",
      |       "Names":["/ecstatic_kalam"],
      |       "Image":"alpine",
      |       "ImageID":"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5",
      |       "Command":"echo hello",
      |       "Created":1666478584,
      |       "Ports":[],
      |       "Labels":{},
      |       "State":"exited",
      |       "Status":"Exited (0) 8 days ago",
      |       "HostConfig":{
      |         "NetworkMode":"default"
      |       },
      |       "NetworkSettings":{
      |         "Networks":{
      |           "bridge":{
      |             "NetworkID":"89e112550a285c17d70fea06881feb46212aa788180181344e0d95158b88b10f",
      |             "EndpointID":"",
      |             "Gateway":"",
      |             "IPAddress":"",
      |             "IPPrefixLen":0,
      |             "IPv6Gateway":"",
      |             "GlobalIPv6Address":"",
      |             "GlobalIPv6PrefixLen":0,
      |             "MacAddress":""
      |           }
      |         }
      |       }
      |     }
      |   ]
      |}
      |""".stripMargin

  sealed trait JsonTreeReader {
    def parent:Option[JsonTreeReader]
    def read(it: TokenIterator):JsonTreeReader

    def accept(objectReader: ObjectReader):JsonTreeReader = {
      parent.map( p => p.accept(objectReader) ).getOrElse(this)
    }
    def accept(arrayReader: ArrayReader):JsonTreeReader = {
      parent.map( p => p.accept(arrayReader) ).getOrElse(this)
    }
    def accept(fieldReader: FieldReader, value:Any):JsonTreeReader = {
      parent.map( p => p.accept(fieldReader,value) ).getOrElse(this)
    }

    def path:List[JsonTreeReader] = {
      var pth = List[JsonTreeReader]()
      var p:Option[JsonTreeReader] = Some(this)
      while( p.isDefined ){
        pth = p.get :: pth
        p = p.get.parent
      }
      pth
    }
  }

  case object NULL

  class InitialReader extends JsonTreeReader {
    var items:List[Any] = List[Any]()
    override def parent: Option[JsonTreeReader] = None
    override def read(it: TokenIterator): JsonTreeReader = {
      it.currentToken() match {
        case Token.ArrayStartToken => new ArrayReader(Some(this))
        case Token.ObjectStartToken => new ObjectReader(Some(this))

        case Token.StringValueToken =>
          items = items :+ it.number()
          this
        case Token.NumberValueToken =>
          items = items :+ it.number()
          this
        case Token.BooleanValueToken =>
          items = items :+ it.boolean()
          this
        case Token.NullValueToken =>
          items = items :+ NULL
          this

        case Token.ArrayEndToken
             | Token.FieldNameToken
             | Token.ObjectEndToken
             | Token.Empty => new FailStateReader(Some(this),it.currentToken())
      }
    }

    override def accept(objectReader: ObjectReader): JsonTreeReader = {
      items = items :+ objectReader
      this
    }
    override def accept(arrayReader: ArrayReader): JsonTreeReader = {
      items = items :+ arrayReader
      this
    }

    override def toString = s"InitialReader()"
  }

  val idSeq = new AtomicInteger(0)

  class ObjectReader(val parent:Option[JsonTreeReader]) extends JsonTreeReader {
    val id = idSeq.getAndIncrement()
    var fields: Map[String,Any] = Map()

    override def read(it: TokenIterator): JsonTreeReader = {
      it.currentToken() match {
        case Token.ObjectEndToken =>
          accept(this)
        case Token.FieldNameToken => new FieldReader(Some(this),it.fieldName())
        //
        case Token.StringValueToken
        | Token.NumberValueToken
        | Token.BooleanValueToken
        | Token.NullValueToken
        | Token.ArrayStartToken
        | Token.ArrayEndToken
        | Token.ObjectStartToken
        | Token.Empty => new FailStateReader(Some(this),it.currentToken())
      }
    }

    override def accept(fieldReader: FieldReader, value: Any): JsonTreeReader = {
      fields = fields + (fieldReader.fieldName -> value)
      //println(s"[obj#$id] accept field ${fieldReader.fieldName} = $value")
      this
    }

    override def toString = s"ObjectReader()"
  }

  class FieldReader(val parent:Option[JsonTreeReader], val fieldName:String) extends JsonTreeReader {
    override def read(it: TokenIterator): JsonTreeReader = {
      it.currentToken() match {
        case Token.StringValueToken =>
          accept(this,it.string())
        case Token.NumberValueToken =>
          accept(this,it.number())
        case Token.BooleanValueToken =>
          accept(this,it.boolean())
        case Token.NullValueToken =>
          accept(this, NULL)
        case Token.ArrayStartToken => new ArrayReader(Some(this))
        case Token.ObjectStartToken => new ObjectReader(Some(this))
        case Token.FieldNameToken | Token.ArrayEndToken | Token.ObjectEndToken | Token.Empty => new FailStateReader(Some(this),it.currentToken())
      }
    }

    override def accept(objectReader: ObjectReader): JsonTreeReader = {
      accept(this, objectReader)
    }

    override def accept(arrayReader: ArrayReader): JsonTreeReader = {
      accept(this, arrayReader)
    }

    override def toString = s"FieldReader(${fieldName})"
  }

  class ArrayReader(val parent:Option[JsonTreeReader]) extends JsonTreeReader {
    var items: List[Any] = List()
    override def read(it: TokenIterator): JsonTreeReader = {
      it.currentToken() match {
        case Token.StringValueToken =>
          items = items :+ it.string()
          this
        case Token.NumberValueToken =>
          items = items :+ it.number()
          this
        case Token.BooleanValueToken =>
          items = items :+ it.boolean()
          this
        case Token.NullValueToken =>
          items = items :+ (NULL)
          this
        case Token.ArrayStartToken => new ArrayReader(Some(this))
        case Token.ArrayEndToken =>
          super.accept(this)
        case Token.ObjectStartToken => new ObjectReader(Some(this))
        case Token.ObjectEndToken | Token.FieldNameToken | Token.Empty => new FailStateReader(Some(this),it.currentToken())
      }
    }

    override def accept(objectReader: ObjectReader): JsonTreeReader = {
      items = items :+ objectReader
      this
    }
    override def accept(arrayReader: ArrayReader): JsonTreeReader = {
      items = items :+ arrayReader
      this
    }

    override def toString = s"ArrayReader()"
  }

  class FailStateReader(val parent:Option[JsonTreeReader], val token: Token) extends JsonTreeReader {
    override def read(it: TokenIterator): JsonTreeReader = {
      this
    }

    override def toString = s"FailStateReader()"
  }

  implicit def read:JsonReader[InitialReader] = new JsonReader[InitialReader] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): InitialReader = {
      var state : JsonTreeReader = new InitialReader()
      var stop = false
      while( !stop ){
        state match {
          case s: InitialReader =>
            if( s.items.nonEmpty ){
              stop = true
            }else{
              state = state.read(it)
              it.next()
            }
          case s: FailStateReader =>
            stop = true
          case _ =>
            //println(s"state ${state} token ${it.currentToken()}")
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

  test("test read log entry") {
    val tree = logEntry.jsonAs[InitialReader]
    println(tree)
  }
}
