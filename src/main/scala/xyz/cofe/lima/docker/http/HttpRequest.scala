package xyz.cofe.lima.docker.http

import java.net.{URL, URLEncoder}
import java.nio.charset.StandardCharsets

case class HttpRequest(
  method:String = "GET",
  path:String,
  proto:String = "HTTP/1.1",
  host:String = "dummy",
  Accept:String = "*/*",
  `User-Agent`:String = "curl/7.79.1",
  otherHeaders:List[(String,String)] = List(),
  body:Seq[Byte] = List()
) {
  def queryString(map:Map[String,String]):HttpRequest = {
    val qs = map.foldLeft(""){ case (str, (key,value)) =>
      val pair =
        URLEncoder.encode(key,StandardCharsets.UTF_8) + "=" +
        URLEncoder.encode(value,StandardCharsets.UTF_8)

      if( str.nonEmpty )
        str + "&" + pair
      else
        pair
    }

    val pathWithoutQuery = {
      val idx = path.indexOf("?")
      if( idx<0 ){
        path
      }else{
        path.substring(0,idx)
      }
    }

    if( qs.isEmpty ){
      copy(path = pathWithoutQuery)
    }else{
      copy(path = pathWithoutQuery + "?" + qs)
    }
  }
  lazy val dump:String = {
    val sb = new StringBuilder
    sb ++= "HttpRequest\n"
    sb ++= s"method $method\n"
    sb ++= s"path $path\n"
    sb ++= s"proto $proto\n"
    sb ++= s"host $host\n"
    sb ++= s"Accept $Accept\n"
    sb ++= s"User-Agent ${`User-Agent`}"
    otherHeaders.foreach { case(k,v) => sb ++= s"otherHeader $k $v\n" }
    sb ++= s"body-size ${body.size}\n"
    val digits = "0123456789abcdef"
    sb ++= body.grouped(32).map { block =>
      "body-data " + block.map { b =>
        val lo = b & 0x0f
        val hi = (b & 0xf0) >> 4
        digits.charAt(hi).toString + digits.charAt(lo).toString
      }.mkString(" ")
    }.mkString("\n")
    sb.toString()
  }
}
