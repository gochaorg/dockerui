package xyz.cofe.lima.docker.http

trait HttpParamValue[V] {
  def httpParamValue(v:V):Option[String]
}
object HttpParamValue {
  //implicit val stringOpt:HttpParamValue[Option[String]] = value => value
  implicit val string:HttpParamValue[String] = value => Some(value)

  //implicit val boolOpt:HttpParamValue[Option[Boolean]] = value => value.map(_.toString)
  implicit val bool:HttpParamValue[Boolean] = value => Some(value.toString)

  //implicit val intOpt:HttpParamValue[Option[Int]] = value => value.map(_.toString)
  implicit val int:HttpParamValue[Int] = value => Some(value.toString)

  //implicit val longOpt:HttpParamValue[Option[Long]] = value => value.map(_.toString)
  implicit val long:HttpParamValue[Long] = value => Some(value.toString)

  implicit def anyOpt[A:HttpParamValue]: HttpParamValue[Option[A]] = value =>
    value.flatMap( v => implicitly[HttpParamValue[A]].httpParamValue(v) )
}
