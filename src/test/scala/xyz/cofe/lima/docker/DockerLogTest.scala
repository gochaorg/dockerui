package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import xyz.cofe.lima.docker.log.Logger
import tethys._
import tethys.jackson._
import tethys.commons.{Token, TokenNode}
import tethys.readers.FieldName
import tethys.readers.tokens.TokenIterator

class DockerLogTest extends AnyFunSuite {
  test("log event") {
    val ev = Logger.ImageCreate()
    println(ev.asJson)
  }

  val logEntry =
    """{"_type":"SuccEvent",
      |"threadId":{"id":18,"name":"JavaFX Application Thread"},
      |"beginTime":"2022-10-31T04:01:21.057027419",
      |"endTime":"2022-10-31T04:01:21.141861409",
      |"args":{
      |"_type":"Containers","all":true,"size":false
      |},
      |
      |"result":[{"Id":"53e9e78d58eff78b7e162c399284c195c4cdc3f793408d62ba336f9ef2dcedd4",
      |"Names":["/n1"],"Image":"alpine","ImageID":"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5",
      |"Command":"echo test",
      |"Created":1666480250,
      |"Ports":[],"Labels":{},
      |"State":"exited","Status":"Exited (0) 8 days ago",
      |"HostConfig":{"NetworkMode":"default"},"NetworkSettings":
      |{"Networks":{"bridge":{"NetworkID":"89e112550a285c17d70fea06881feb46212aa788180181344e0d95158b88b10f",
      |"EndpointID":"","Gateway":"","IPAddress":"",
      |"IPPrefixLen":0,"IPv6Gateway":"","GlobalIPv6Address":"",
      |"GlobalIPv6PrefixLen":0,"MacAddress":""}}}},
      |{"Id":"1668f9b53221b7cc09beb97c162a23ead66d11a1fd0127bcb10228869d55da79",
      |"Names":["/ecstatic_kalam"],"Image":"alpine","ImageID":"sha256:9c6f0724472873bb50a2ae67a9e7adcb57673a183cea8b06eb778dca859181b5",
      |"Command":"echo hello","Created":1666478584,"Ports":[],
      |"Labels":{},"State":"exited","Status":"Exited (0) 8 days ago",
      |"HostConfig":{"NetworkMode":"default"},"NetworkSettings":
      |{"Networks":{"bridge":{"NetworkID":"89e112550a285c17d70fea06881feb46212aa788180181344e0d95158b88b10f",
      |"EndpointID":"","Gateway":"","IPAddress":"","IPPrefixLen":0,"IPv6Gateway":"",
      |"GlobalIPv6Address":"","GlobalIPv6PrefixLen":0,"MacAddress":""}}}}]}""".stripMargin

  case class CaptureObj(from: Int, to: Int, typeName: String, fields: Map[String, Any])
  case object NULL
  case class Raw( tokens:List[TokenNode], obj:Option[CaptureObj] )

  implicit def read:JsonReader[Raw] = new JsonReader[Raw] {
    override def read(it: TokenIterator)(implicit fieldName: FieldName): Raw = {
      var stop = false
      var state = 0

      var tokenNodes = List[TokenNode]()
      var level = 0

      var objectStack = List[Int]()
      var objectType = List[String]()

      var fieldName = List[String]()
      var fieldBegin = List[Int]()
      var fieldCapture = List[Boolean]()
      var lastObject :Option[CaptureObj] = None

      var currentObjectFields = List[Map[String,Any]]()

      def captureObject( from:Int, to:Int, typeName:String )={
        println(s"from=$from to=$to $typeName")
        val fields = currentObjectFields.head
        println(fields)
        CaptureObj(from, to, typeName, fields)
      }

      def captureFieldStr(name: String, value: String): Unit = {
        if (currentObjectFields.nonEmpty) {
          currentObjectFields = (currentObjectFields.head + (name -> value)) :: currentObjectFields.tail
        }
      }
      def captureFieldBool( name:String, value:Boolean ):Unit = {
        if( currentObjectFields.nonEmpty ){
          currentObjectFields = (currentObjectFields.head + (name -> value)) :: currentObjectFields.tail
        }
      }
      def captureFieldNum( name:String, value:Number ):Unit = {
        if( currentObjectFields.nonEmpty ){
          currentObjectFields = (currentObjectFields.head + (name -> value)) :: currentObjectFields.tail
        }
      }
      def captureFieldObj( name:String, captureObj: CaptureObj ):Unit = {
        if (currentObjectFields.nonEmpty) {
          currentObjectFields = (currentObjectFields.head + (name -> captureObj)) :: currentObjectFields.tail
        }
      }
      def captureFieldArr( from:Int, to:Int, name:String, arr:List[Any] ):Unit = {
        if (currentObjectFields.nonEmpty) {
          currentObjectFields = (currentObjectFields.head + (name -> arr)) :: currentObjectFields.tail
        }
      }

      var arrayCapture = List[Boolean]()
      var arrayItem = List[List[Any]]()

      while(!stop) {
        state match {
          case 0 =>
            it.currentToken() match {
              case Token.ObjectStartToken =>
                level+=1
                tokenNodes = tokenNodes :+ TokenNode.ObjectStartNode

                objectStack = (tokenNodes.length-1) :: objectStack
                objectType = "?" :: objectType
                currentObjectFields = Map[String,Any]() :: currentObjectFields

                state = 1
                it.next()
              case _ => state = -1
            }
          case 1 =>
            it.currentToken() match {
              case Token.ObjectStartToken =>
                level += 1
                tokenNodes =  tokenNodes :+ TokenNode.ObjectStartNode

                objectStack = (tokenNodes.length - 1) :: objectStack
                objectType = "?" :: objectType
                currentObjectFields = Map[String,Any]() :: currentObjectFields

                it.next()
              case Token.ObjectEndToken =>
                level -= 1
                tokenNodes = tokenNodes :+ TokenNode.ObjectEndNode
                if (level == 0) {
                  stop = false
                }

                val objBegin = objectStack.head
                val objEnd = tokenNodes.length-1
                val objType = objectType.head
                val capObj = captureObject(objBegin,objEnd,objType)

                objectStack = objectStack.tail
                objectType = objectType.tail
                currentObjectFields = currentObjectFields.tail

                if (fieldCapture.headOption.getOrElse(false)) {
                  val fName = fieldName.head
                  val fBegin = fieldBegin.head
                  val fEnd = tokenNodes.length - 1
                  fieldCapture = fieldCapture.tail
                  fieldBegin = fieldBegin.tail
                  fieldName = fieldName.tail
                  captureFieldObj(fName, capObj)
                }

                if(arrayCapture.headOption.getOrElse(false)){
                  arrayItem = (arrayItem.head ++ List(capObj)):: arrayItem.tail
                }

                if( level==0 ){
                  lastObject = Some(capObj)
                  stop = true
                }else {
                  it.next()
                }

              case Token.FieldNameToken =>
                tokenNodes = tokenNodes :+ TokenNode.FieldNameNode(it.fieldName())

                fieldBegin = tokenNodes.length - 1 :: fieldBegin
                fieldName = it.fieldName() :: fieldName
                fieldCapture = true :: fieldCapture

                it.next()

              case Token.StringValueToken =>
                tokenNodes = tokenNodes :+ TokenNode.StringValueNode(it.string())

                if (tokenNodes.length > 1 && tokenNodes(tokenNodes.length - 2).isInstanceOf[TokenNode.FieldNameNode]) {
                  val fname = tokenNodes(tokenNodes.length - 2).asInstanceOf[TokenNode.FieldNameNode].value
                  if (fname == "_type" && objectType.nonEmpty) {
                    objectType = it.string() :: objectType.tail
                  }
                }

                if (fieldCapture.headOption.getOrElse(false)) {
                  val fName = fieldName.head
                  val fBegin = fieldBegin.head
                  val fieldValue = it.string()
                  val fEnd = tokenNodes.length - 1
                  fieldCapture = fieldCapture.tail
                  fieldBegin = fieldBegin.tail
                  fieldName = fieldName.tail
                  captureFieldStr(fName, fieldValue)
                }

                if (arrayCapture.headOption.getOrElse(false)) {
                  arrayItem = (arrayItem.head ++ List(it.string())) :: arrayItem.tail
                }

                it.next()

              case Token.NumberValueToken =>
                tokenNodes = tokenNodes :+ TokenNode.NumberValueNode(it.number())

                if( fieldCapture.headOption.getOrElse(false) ){
                  val fName = fieldName.head
                  val fBegin = fieldBegin.head
                  val fieldValue = it.number()
                  val fEnd = tokenNodes.length-1
                  fieldCapture = fieldCapture.tail
                  fieldBegin = fieldBegin.tail
                  fieldName = fieldName.tail
                  captureFieldNum(fName, fieldValue)
                }

                if (arrayCapture.headOption.getOrElse(false)) {
                  arrayItem = (arrayItem.head ++ List(it.number())) :: arrayItem.tail
                }

                it.next()
              case Token.BooleanValueToken =>
                tokenNodes = tokenNodes :+ TokenNode.BooleanValueNode(it.boolean())

                if (fieldCapture.headOption.getOrElse(false)) {
                  val fName = fieldName.head
                  val fBegin = fieldBegin.head
                  val fieldValue = it.boolean()
                  val fEnd = tokenNodes.length-1
                  fieldCapture = fieldCapture.tail
                  fieldBegin = fieldBegin.tail
                  fieldName = fieldName.tail
                  captureFieldBool(fName, fieldValue)
                }

                if (arrayCapture.headOption.getOrElse(false)) {
                  arrayItem = (arrayItem.head ++ List(it.boolean())) :: arrayItem.tail
                }

                it.next()
              case Token.NullValueToken =>
                tokenNodes = tokenNodes :+ TokenNode.NullValueNode
                if (fieldCapture.headOption.getOrElse(false)) {
                  val fName = fieldName.head
                  val fBegin = fieldBegin.head
                  val fieldValue = null
                  val fEnd = tokenNodes.length-1
                  fieldCapture = fieldCapture.tail
                  fieldBegin = fieldBegin.tail
                  fieldName = fieldName.tail
                }

                if (arrayCapture.headOption.getOrElse(false)) {
                  arrayItem = (arrayItem.head ++ List(NULL)) :: arrayItem.tail
                }

                it.next()
              case Token.ArrayEndToken =>
                tokenNodes = tokenNodes :+ TokenNode.ArrayEndNode

                val items = arrayCapture.headOption.map { cap =>
                  if( cap ) {
                    arrayItem.head
                  }else{ List() }
                }.getOrElse( List() )

                if( arrayCapture.headOption.getOrElse(false) ){
                  arrayCapture = arrayCapture.tail
                  arrayItem = arrayItem.tail
                }

                if (fieldCapture.headOption.getOrElse(false)) {
                  val fName = fieldName.head
                  val fBegin = fieldBegin.head
                  val fEnd = tokenNodes.length-1
                  fieldCapture = fieldCapture.tail
                  fieldBegin = fieldBegin.tail
                  fieldName = fieldName.tail
                  captureFieldArr(fBegin, fEnd, fName, items)
                }

                it.next()
              case Token.ArrayStartToken =>
                tokenNodes = tokenNodes :+ TokenNode.ArrayStartNode

                arrayCapture = true :: arrayCapture
                arrayItem = List() :: arrayItem

                it.next()
            }
          case (-1) =>
            stop = true
        }
      }

      Raw(tokenNodes, lastObject)
    }
  }

  test("log test") {
    val raw = logEntry.jsonAs[Raw]
    println(raw)
  }
}
