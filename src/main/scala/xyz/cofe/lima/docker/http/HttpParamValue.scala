package xyz.cofe.lima.docker.http

trait HttpParamValue[V] {
  def httpParamValue(v:V):Option[String]
}
object HttpParamValue {
  implicit val string:HttpParamValue[String] = value => Some(value)
  implicit val bool:HttpParamValue[Boolean] = value => Some(value.toString)
  implicit val int:HttpParamValue[Int] = value => Some(value.toString)
  implicit val long:HttpParamValue[Long] = value => Some(value.toString)

  implicit def anyOpt[A:HttpParamValue]: HttpParamValue[Option[A]] = value =>
    value.flatMap( v => implicitly[HttpParamValue[A]].httpParamValue(v) )
}
