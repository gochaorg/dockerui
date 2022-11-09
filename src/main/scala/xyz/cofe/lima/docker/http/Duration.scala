package xyz.cofe.lima.docker.http

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

import java.util.regex.Pattern

/**
 * Задержка
 *
 * @param ms мс, 0 или больше
 * @param nanos наносек, 0 или больше
 */
case class Duration(ms:Long, nanos:Int=0) extends Ordered[Duration] {
  require(ms>=0)
  require(nanos>=0)

  /**
   * задержка (Thread sleep)
   */
  def sleep():Unit = {
    if( ms>0 || nanos>0 ){
      Thread.sleep(ms,nanos)
    }
  }

  def some:Option[Duration] = Some(this)

  def normalize:Duration = {
    if( nanos>= 1000000 ){
      val addMs = (nanos / 1000000)
      val nanoz = (nanos % 1000000).toInt
      Duration( ms+addMs, nanoz )
    }else{
      this
    }
  }

  def +(that:Duration):Duration = {
    Duration(ms=ms+that.ms, nanos=nanos+that.nanos).normalize
  }

  override def compare(that: Duration): Int = {
    if( ms==that.ms ){
      nanos.compare(that.nanos)
    }else{
      ms.compare(that.ms)
    }
  }

  def >( that:Option[Duration] ):Boolean = {
    that match {
      case Some(value) => this.compare(value) > 0
      case None => false
    }
  }
  def <( that:Option[Duration] ):Boolean = {
    that match {
      case Some(value) => this.compare(value) < 0
      case None => false
    }
  }
  def ==( that:Option[Duration] ):Boolean = {
    that match {
      case Some(value) => this.compare(value)==0
      case None => false
    }
  }
  def !=( that:Option[Duration] ):Boolean = {
    that match {
      case Some(value) => this.compare(value)!=0
      case None => false
    }
  }
  def >=(that: Option[Duration]): Boolean = {
    that match {
      case Some(value) => this.compare(value) >= 0
      case None => false
    }
  }
  def <=(that: Option[Duration]): Boolean = {
    that match {
      case Some(value) => this.compare(value) <= 0
      case None => false
    }
  }

  override def toString(): String = {
    val dayMs = 1000L * 60L * 60L * 24L
    val hourMs = 1000L * 60L * 60L
    val minMs = 1000L * 60L
    val secMs = 1000L

    val days = ms / dayMs
    val hours = (ms - dayMs * days) / hourMs
    val minutes = (ms - dayMs * days - hours * hourMs) / minMs
    val seconds = (ms - dayMs * days - hours * hourMs - minutes * minMs) / secMs
    val mseconds = (ms - dayMs * days - hours * hourMs - minutes * minMs - seconds * secMs) % secMs

    List(
      days.toInt -> "days",
      hours.toInt -> "hours",
      minutes.toInt -> "minutes",
      seconds.toInt -> "seconds",
      mseconds.toInt -> "ms",
      nanos -> "ns"
    ).filter { case (i, _) => i > 0 }
      .map { case (i, p) => s"$i $p" }
      .mkString(" ")
  }
}

object Duration {
  case class TimePoint(nanoTime:Long) extends AnyVal {
    def duration(point:TimePoint):Duration = {
      val t_a = nanoTime
      val t_b = point.nanoTime
      val d_nano_sum = Math.abs(t_a - t_b)
      val d_millis = d_nano_sum / 1000000L
      val d_nanos = (d_nano_sum % 1000000L).toInt
      Duration(d_millis, d_nanos)
    }

    def -(point:TimePoint):Duration = {
      duration(point)
    }

    def some:Option[TimePoint] = Some(this)
  }
  object TimePoint {
    def now:TimePoint = TimePoint(System.nanoTime())
  }

  implicit class IntOps( val value:Int ) extends AnyVal {
    def nanosec:Duration = Duration(0,value)
    def milliseconds:Duration = Duration(value)
    def seconds:Duration = Duration(value*1000L)
    def minutes:Duration = Duration(value*1000L*60L)
    def hours:Duration = Duration(value*1000L*60L*60L)
    def days:Duration = Duration(value*1000L*60L*60L*24L)
  }
  implicit class LongOps( val value:Long ) extends AnyVal {
    def nanosec:Duration = Duration(value/1000000L,(value%1000000L).toInt)
    def milliseconds:Duration = Duration(value)
    def seconds:Duration = Duration(value*1000L)
    def minutes:Duration = Duration(value*1000L*60L)
    def hours:Duration = Duration(value*1000L*60L*60L)
    def days:Duration = Duration(value*1000L*60L*60L*24L)
  }

  implicit class DurationOptOps( val duration:Option[Duration] ) extends AnyVal {
    def sleep():Unit = duration.foreach(_.sleep())
  }

  implicit val reader: JsonReader[Duration] = jsonReader[Duration]
  implicit val writer: JsonWriter[Duration] = jsonWriter[Duration]

  private def ptrnUnit(value:String,unit:String):String = s"((?<$value>\\d+)\\s*(?<$unit>n[anos]{0,4}|m[li]{0,4}s|se[conds]{0,5}|min[untes]{0,5}|h[ours]{0,4}|d[ays]{0,3}))"
  private val ptrn:Pattern = Pattern.compile("(?is)"+
    ptrnUnit("v0","u0")+
    "((\\s+)"+ptrnUnit("v1","u1")+
    "((\\s+)"+ptrnUnit("v2","u2")+
    "((\\s+)"+ptrnUnit("v3","u3")+
    "((\\s+)"+ptrnUnit("v4","u4")+
    "((\\s+)"+ptrnUnit("v5","u5")+
    "((\\s+)"+ptrnUnit("v6","u6")+
    "((\\s+)"+ptrnUnit("v7","u7")+
    "((\\s+)"+ptrnUnit("v8","u8")+
    "((\\s+)"+ptrnUnit("v9","u9")+")?)?)?)?)?)?)?)?)?"+
    ""
  )
  private def toUnit(value:Int,unitName:String):Duration = {
    unitName match {
      case s:String if s.matches("(?is)n[anos]{0,4}") => value.nanosec
      case s:String if s.matches("(?is)m[li]{0,4}s") => value.milliseconds
      case s:String if s.matches("(?is)se[conds]{0,5}") => value.seconds
      case s:String if s.matches("(?is)min[untes]{0,5}") => value.minutes
      case s:String if s.matches("(?is)h[ours]{0,4}") => value.hours
      case s:String if s.matches("(?is)d[ays]{0,3}") => value.days
      case null | "" => value.milliseconds
    }
  }

  def parse(str:String):Either[String,Duration] = {
    val m = ptrn.matcher(str)
    if( m.matches() ){
      Right((0 to 9).map{ gi =>
        val num = m.group(s"v$gi")
        val unt = m.group(s"u$gi")
        num match {
          case null => None
          case str => unt match {
            case null => Some(num.toInt,"ms")
            case u => Some(num.toInt,u)
          }
        }
      }.filter(_.isDefined)
        .map(_.get)
        .map(x=>toUnit(x._1, x._2))
        .foldLeft(Duration(0)){ case(sum, d) => sum + d })
    }else{
      Left("not parsed, await follow pattern \\d+\\s*(ms|sec|hour|minute|hour|day)")
    }
  }
}
