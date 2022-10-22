package xyz.cofe.lima.docker.http

import tethys.JsonWriter

import java.net.{URL, URLEncoder}
import java.nio.charset.{Charset, StandardCharsets}
import tethys._
import tethys.jackson._

import java.io.ByteArrayOutputStream

case class HttpRequest(
  path:String,
  method:String = "GET",
  proto:String = "HTTP/1.1",
  host:String = "dummy",
  Accept:String = "*/*",
  `User-Agent`:String = "curl/7.79.1",
  otherHeaders:List[(String,String)] = List(),
  body:Seq[Byte] = List()
) {
  def get():HttpRequest = copy(method="GET")
  def post():HttpRequest = copy(method="POST")
  def delete():HttpRequest = copy(method="DELETE")
  def head():HttpRequest = copy(method="HEAD")
  def put():HttpRequest = copy(method="PUT")
  def options():HttpRequest = copy(method="OPTIONS")
  def patch():HttpRequest = copy(method="PATCH")

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
  def queryString[V:HttpParamValue](args:(String,V)*):HttpRequest = {
    queryString(
      args
        .map { case(k,v) => (k,implicitly[HttpParamValue[V]].httpParamValue(v)) }
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
        .toMap
    )
  }

  def header(name:String, value:String):HttpRequest = {
    if( name.equalsIgnoreCase("Accept") ){
      copy(Accept=value)
    }else if( name.equalsIgnoreCase("User-Agent") ){
      copy(`User-Agent`=value)
    }else{
      copy(
        otherHeaders =
          otherHeaders
            .filterNot { case (hname,_) => hname.equalsIgnoreCase(name) } ++
            List((name, value))
      )
    }
  }

  def bodyText(string: String, charset:Option[Charset]=None, computeContentLength:Boolean=true):HttpRequest = {
    val cs = charset.getOrElse(StandardCharsets.UTF_8)
    val bytes = string.getBytes(cs)
    if( computeContentLength ){
      copy(
        body = bytes
      ).header("Content-Length", bytes.length.toString)
    }else{
      copy(
        body = bytes
      )
    }
  }

  def json(string: String):HttpRequest =
    bodyText(string).header("Content-type","application/json")

  def json[A:JsonWriter](jsonObj:A):HttpRequest =
    bodyText(jsonObj.asJson).header("Content-type","application/json")

  lazy val dump:String = {
    val sb = new StringBuilder
    sb ++= "HttpRequest\n"
    sb ++= s"method $method\n"
    sb ++= s"path $path\n"
    sb ++= s"proto $proto\n"
    sb ++= s"host $host\n"
    sb ++= s"header: Accept: $Accept\n"
    sb ++= s"header: User-Agent: ${`User-Agent`}\n"
    otherHeaders.foreach { case(k,v) => sb ++= s"header: $k: $v\n" }
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

  def toBytes:Array[Byte] = {
    val ba = new ByteArrayOutputStream();

    val headerBlock =
      ((method + " " + path + " " + proto + "\n") +
        ("HOST: " + host + "\n") +
        ("User-Agent: " + `User-Agent` + "\n") +
        ("Accept: " + Accept + "\n") +
        (otherHeaders.map { case (k, v) => k + ": " + v }.mkString("\n"))
        +"\n\n"
        ).getBytes(StandardCharsets.UTF_8)

    ba.write(headerBlock)
    ba.write(body.toArray)

    ba.toByteArray
  }
}
