package xyz.cofe.lima.docker.http

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

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
    def milliseconds:Duration = Duration(value)
    def seconds:Duration = Duration(value*1000L)
  }
  implicit class LongOps( val value:Long ) extends AnyVal {
    def milliseconds:Duration = Duration(value)
    def seconds:Duration = Duration(value*1000L)
  }

  implicit class DurationOptOps( val duration:Option[Duration] ) extends AnyVal {
    def sleep():Unit = duration.foreach(_.sleep())
  }

  implicit val reader: JsonReader[Duration] = jsonReader[Duration]
  implicit val writer: JsonWriter[Duration] = jsonWriter[Duration]
}
