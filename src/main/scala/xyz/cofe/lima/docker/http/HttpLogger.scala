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
        case Event.Error(pid,string) => println(s"ev.err $string")
        case Event.FirstLine(pid,string) => println(s"ev.firstLine $string")
        case Event.Header(pid,name, value) => println(s"ev.header $name: $value")
        case Event.HeaderEnd(pid) => println("ev.header end")
        case Event.Data(pid,bytes) =>
          if(logStreamData){
            println(HexDump.toString(bytes,"ev.data bytes "))
          }
        case Event.DataEnd(pid) => println("ev.data end")
      }
      ev
    }
    override def error(string: String): Unit = println(s"err $string")
  }

  class JoinLoggers(loggers: Seq[HttpLogger]) extends HttpLogger {
    override def send(httpRequest: HttpRequest): Unit = {
      loggers.foreach(_.send(httpRequest))
    }
    override def receive(httpResponse: HttpResponse): Unit = {
      loggers.foreach(_.receive(httpResponse))
    }
    override def event(event: Event): Event = {
      loggers.foreach(_.event(event))
      event
    }
    override def error(string: String): Unit = {
      loggers.foreach(_.error(string))
    }
  }

  def join( loggerA:HttpLogger, loggerB:HttpLogger, loggers:HttpLogger* ):JoinLoggers = new JoinLoggers( List(loggerA) ++ List(loggerB) ++ loggers)
  def join( loggers:Seq[HttpLogger] ):JoinLoggers = new JoinLoggers(loggers)
}
