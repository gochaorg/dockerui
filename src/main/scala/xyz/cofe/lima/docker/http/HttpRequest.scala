package xyz.cofe.lima.docker.http

import tethys.JsonWriter

import java.net.{URL, URLEncoder}
import java.nio.charset.{Charset, StandardCharsets}
import tethys._
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import tethys.jackson._
import xyz.cofe.lima.thread.ThreadID

import java.io.ByteArrayOutputStream
import java.time.LocalDateTime
import java.util.concurrent.atomic.AtomicLong
import xyz.cofe.lima.store.json._

case class HttpRequest(
  path:String,
  method:String = "GET",
  proto:String = "HTTP/1.1",
  host:String = "dummy",
  Accept:String = "*/*",
  `User-Agent`:String = "curl/7.79.1",
  otherHeaders:List[(String,String)] = List(),
  body:Seq[Byte] = List(),
  id: Long = HttpRequest.idSeq.incrementAndGet()
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

  def queryString[
    A0:HttpParamValue,
  ](a0:(String,A0) ):HttpRequest = {
    queryString(
      Map(
        a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
      )
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
    )
  }

  def queryString[
    A0:HttpParamValue,
    A1:HttpParamValue,
  ](a0:(String,A0), a1:(String,A1) ):HttpRequest = {
    queryString(
      Map(
        a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
      )
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
    )
  }

  def queryString[
    A0:HttpParamValue,
    A1:HttpParamValue,
    A2:HttpParamValue
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2) ):HttpRequest = {
    queryString(
      Map(
        a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
        a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
      )
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
    )
  }

  def queryString[
    A0:HttpParamValue,
    A1:HttpParamValue,
    A2:HttpParamValue,
    A3:HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3) ):HttpRequest = {
    queryString(
      Map(
        a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
        a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
        a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
      )
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
    )
  }

  def queryString[
    A0:HttpParamValue,
    A1:HttpParamValue,
    A2:HttpParamValue,
    A3:HttpParamValue,
    A4:HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3), a4:(String,A4) ):HttpRequest = {
    queryString(
      Map(
        a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
        a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
        a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
        a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
      )
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
    )
  }

  def queryString[
    A0:HttpParamValue,
    A1:HttpParamValue,
    A2:HttpParamValue,
    A3:HttpParamValue,
    A4:HttpParamValue,
    A5:HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3), a4:(String,A4), a5:(String,A5) ):HttpRequest = {
    queryString(
      Map(
        a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
        a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
        a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
        a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
        a5._1 -> implicitly[HttpParamValue[A5]].httpParamValue(a5._2),
      )
        .filter { case(_,v) => v.nonEmpty }
        .map { case(k,v) => (k,v.get) }
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
    sb ++= s"id $id\n"
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

object HttpRequest {
  lazy val idSeq = new AtomicLong()

  case class HttpRequestLogView(threadId:ThreadID,
                                time:LocalDateTime,
                                path:String,
                                method:String,
                                proto: String,
                                headers: Map[String,String],
                                body: Option[String],
                                id: Long
                               ) {
    def toRequest:HttpRequest =
      HttpRequest(
        path = path,
        method = method,
        proto = proto,
        id = id,
        host = headers.getOrElse("Host", "dummy"),
        Accept = headers.getOrElse("Accept", "dummy"),
        `User-Agent` = headers.getOrElse("User-Agent", "dummy"),
        otherHeaders = headers.filter(_._1 != "Host").filter(_._1!="Accept").filter(_._1!="User-Agent").toList,
        body = body match {
          case Some(value) => HexDump.bytesFrom(value)
          case None => null
        }
      )
  }

  object HttpRequestLogView {
    def apply(req:HttpRequest):HttpRequestLogView =
      new HttpRequestLogView(
        threadId=ThreadID.current,
        time=LocalDateTime.now(),
        path=req.path,
        proto = req.proto,
        id = req.id,
        method = req.method,
        body = if(req.body!=null){
          Some(HexDump.toString(req.body))
        }else{
          None
        },
        headers = Map(
          "Host" -> req.host,
          "Accept" -> req.Accept,
          "User-Agent" -> req.`User-Agent`
        ) ++ req.otherHeaders.toMap
      )

    implicit val writer: JsonWriter[HttpRequestLogView] = jsonWriter[HttpRequestLogView]
    implicit val reader: JsonReader[HttpRequestLogView] = jsonReader[HttpRequestLogView]
  }

  implicit val writer:JsonWriter[HttpRequest] =
    HttpRequestLogView.writer.contramap[HttpRequest]( req => HttpRequestLogView(req) )

  implicit val reader:JsonReader[HttpRequest] =
    HttpRequestLogView.reader.map[HttpRequest]( req => req.toRequest )
}
