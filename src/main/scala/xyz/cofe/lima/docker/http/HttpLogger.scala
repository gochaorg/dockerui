package xyz.cofe.lima.docker.http

import xyz.cofe.lima.docker.http.HttpResponseStream.Event

trait HttpLogger {
  def send(httpRequest: HttpRequest):Unit
  def receive(httpResponse: HttpResponse):Unit
  def event(event:HttpResponseStream.Event):HttpResponseStream.Event
  def error(string: String):Unit
}

object HttpLogger {
  implicit val defaultLogger = new HttpLogger {
    override def send(httpRequest: HttpRequest): Unit = {}
    override def receive(httpResponse: HttpResponse): Unit = {}
    override def event(ev: HttpResponseStream.Event): HttpResponseStream.Event = ev
    override def error(string: String): Unit = ()
  }
  def stdout(logStreamData:Boolean=false):HttpLogger = new HttpLogger {
    override def send(httpRequest: HttpRequest): Unit = {
      println(httpRequest.dump)
    }
    override def receive(httpResponse: HttpResponse): Unit = {
      println("body:")
      println(httpResponse.text)
      println("="*40)
      println("dump")
      println(httpResponse.dump)
    }
    override def event(ev: HttpResponseStream.Event): HttpResponseStream.Event = {
      ev match {
        case Event.Error(string) => println(s"ev.err $string")
        case Event.FirstLine(string) => println(s"ev.firstLine $string")
        case Event.Header(name, value) => println(s"ev.header $name: $value")
        case Event.HeaderEnd => println("ev.header end")
        case Event.Data(bytes) =>
          if(logStreamData){
            println(HexDump.toString(bytes,"ev.data bytes "))
          }
        case Event.DataEnd => println("ev.data end")
      }
      ev
    }
    override def error(string: String): Unit = println(s"err $string")
  }
}
