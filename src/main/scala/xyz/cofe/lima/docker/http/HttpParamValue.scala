package xyz.cofe.lima.docker.http

trait HttpParamValue[V] {
  def httpParamValue(v:V):String
}
object HttpParamValue {
  implicit val string:HttpParamValue[String] = value => value
  implicit val bool:HttpParamValue[Boolean] = value => value.toString
  implicit val int:HttpParamValue[Int] = value => value.toString
  implicit val long:HttpParamValue[Long] = value => value.toString
}
