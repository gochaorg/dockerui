package xyz.cofe.lima.docker

import org.scalatest.funsuite.AnyFunSuite
import tethys._
import tethys.jackson._
import xyz.cofe.lima.docker.log.Logger.Containers
import xyz.cofe.lima.store.json._

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

  test("test read log entry") {
    import xyz.cofe.lima.store.json.Query._
    import xyz.cofe.lima.store.json.TethysToks._

    val tree = logEntry.jsonAs[JsValue]
    println(tree.query("_type").string)
    println(tree.query("args")("_type").string)

    println(tree.query("args").jsObject.map(_.tokens))
    tree.query("args").jsObject.map(_.jsonAs[Containers]).get match {
      case Left(value) => println(value)
      case Right(value) => println(value)
    }

    tree.query("result").jsValue.map(_.jsonAs[Containers#RESULT]) match {
      case Some(value) => value match {
        case Left(value) => ???
        case Right(value) => println(value)
      }
      case None => ???
    }
  }
}
