package xyz.cofe.lima.docker.http

import tethys.JsonWriter
import tethys.derivation.semiauto.{jsonReader, jsonWriter}
import xyz.cofe.lima.docker.http.HttpResponseStream.Event
import xyz.cofe.lima.store.json._
import xyz.cofe.lima.thread.ThreadID
import tethys._
import tethys.jackson._
import tethys.writers.tokens.TokenWriter

import java.time.LocalDateTime

trait HttpLogger {
  def send(httpRequest: HttpRequest):Unit
  def receive(httpResponse: HttpResponse):Unit
  def event(event:HttpResponseStream.Event):HttpResponseStream.Event
  def error(string: String):Unit
}

object HttpLogger {
  implicit val defaultLogger: HttpLogger = new HttpLogger {
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

  sealed trait LogEvent
  object LogEvent {
    case class Send(request:HttpRequest, threadId:ThreadID=ThreadID.current, time:LocalDateTime=LocalDateTime.now()) extends LogEvent
    object Send {
      implicit val writer: JsonWriter[Send] = classWriter[Send] ++ jsonWriter[Send]
      implicit val reader: JsonReader[Send] = jsonReader[Send]
    }

    case class Receive(response:HttpResponse, threadId:ThreadID=ThreadID.current, time:LocalDateTime=LocalDateTime.now()) extends LogEvent
    object Receive {
      implicit val writer: JsonWriter[Receive] = classWriter[Receive] ++ jsonWriter[Receive]
      implicit val reader: JsonReader[Receive] = jsonReader[Receive]
    }

    case class Event(streamEvent:HttpResponseStream.Event, threadId:ThreadID=ThreadID.current, time:LocalDateTime=LocalDateTime.now()) extends LogEvent
    object Event {
      implicit val writer: JsonWriter[Event] = classWriter[Event] ++ jsonWriter[Event]
      implicit val reader: JsonReader[Event] = jsonReader[Event]
    }

    case class Error(message:String, threadId:ThreadID=ThreadID.current, time:LocalDateTime=LocalDateTime.now()) extends LogEvent
    object Error {
      implicit val writer: JsonWriter[Error] = classWriter[Error] ++ jsonWriter[Error]
      implicit val reader: JsonReader[Error] = jsonReader[Error]
    }

    implicit val writer:JsonWriter[LogEvent] = new JsonWriter[LogEvent] {
      override def write(value: LogEvent, tokenWriter: TokenWriter): Unit = {
        value match {
          case ev:Send => Send.writer.write(ev,tokenWriter)
          case ev:Receive => Receive.writer.write(ev,tokenWriter)
          case ev:Event => Event.writer.write(ev,tokenWriter)
          case ev:Error => Error.writer.write(ev,tokenWriter)
        }
      }
    }

    implicit val reader:JsonReader[LogEvent] = JsonReader.builder.addField[String]("_type").selectReader[LogEvent] {
      case "Send" => Send.reader
      case "Receive" => Receive.reader
      case "Event" => Event.reader
      case "Error" => Error.reader
    }
  }

  case class JsonLogger(out:java.lang.Appendable) extends HttpLogger {
    override def send(httpRequest: HttpRequest): Unit = {
      out.synchronized {
        out.append(LogEvent.Send(httpRequest).asJson).append(System.lineSeparator())
      }
    }
    override def receive(httpResponse: HttpResponse): Unit = {
      out.synchronized {
        out.append(LogEvent.Receive(httpResponse).asJson).append(System.lineSeparator())
      }
    }
    override def event(event: Event): Event = {
      out.synchronized {
        out.append(LogEvent.Event(event).asJson).append(System.lineSeparator())
      }
      event
    }
    override def error(string: String): Unit = {
      out.synchronized {
        out.append(LogEvent.Error(string).asJson).append(System.lineSeparator())
      }
    }
  }
}
