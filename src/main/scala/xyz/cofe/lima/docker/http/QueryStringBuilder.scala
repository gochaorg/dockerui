package xyz.cofe.lima.docker.http

import java.net.URLEncoder
import java.nio.charset.StandardCharsets

trait QueryStringBuilder {
  type SELF

  def queryString[
    A0: HttpParamValue,
  ](a0:(String,A0) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
  ](a0:(String,A0), a1:(String,A1) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
    A4: HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3), a4:(String,A4) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
    A4: HttpParamValue,
    A5: HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3), a4:(String,A4), a5:(String,A5) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
    A4: HttpParamValue,
    A5: HttpParamValue,
    A6: HttpParamValue,
  ](a0:(String,A0), a1:(String,A1), a2:(String,A2), a3:(String,A3), a4:(String,A4), a5:(String,A5), a6:(String,A6) ):SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
    A4: HttpParamValue,
    A5: HttpParamValue,
    A6: HttpParamValue,
    A7: HttpParamValue,
  ](a0: (String, A0),
    a1: (String, A1),
    a2: (String, A2),
    a3: (String, A3),
    a4: (String, A4),
    a5: (String, A5),
    a6: (String, A6),
    a7: (String, A7),
   ): SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
    A4: HttpParamValue,
    A5: HttpParamValue,
    A6: HttpParamValue,
    A7: HttpParamValue,
    A8: HttpParamValue,
  ](a0: (String, A0),
    a1: (String, A1),
    a2: (String, A2),
    a3: (String, A3),
    a4: (String, A4),
    a5: (String, A5),
    a6: (String, A6),
    a7: (String, A7),
    a8: (String, A8),
   ): SELF

  def queryString[
    A0: HttpParamValue,
    A1: HttpParamValue,
    A2: HttpParamValue,
    A3: HttpParamValue,
    A4: HttpParamValue,
    A5: HttpParamValue,
    A6: HttpParamValue,
    A7: HttpParamValue,
    A8: HttpParamValue,
    A9: HttpParamValue,
  ]( a0:(String,A0),
     a1:(String,A1),
     a2:(String,A2),
     a3:(String,A3),
     a4:(String,A4),
     a5:(String,A5),
     a6:(String,A6),
     a7:(String,A7),
     a8:(String,A8),
     a9:(String,A9)
  ):SELF
}

object QueryStringBuilder {
  def buildQueryString( entries:List[(String,String)] ):Option[String] = {
    if (entries.isEmpty) {
      None
    } else {
      Some(entries.flatMap { case (name, value) =>
        if (name.nonEmpty) {
          List(URLEncoder.encode(name, StandardCharsets.UTF_8) + "=" + {
            if (value.nonEmpty) {
              URLEncoder.encode(value, StandardCharsets.UTF_8)
            } else {
              ""
            }
          })
        } else {
          List()
        }
      }.mkString("&"))
    }
  }

  def buildQueryStringOpt( entries:List[(String,Option[String])] ):Option[String] =
    buildQueryString( entries.flatMap { case (k,vOpt)=> vOpt.map { v => (k,v) } } )

  def apply():QueryString = new QueryString()

  class QueryString extends QueryStringBuilder {
    type SELF = Option[String]

    def queryString[A0: HttpParamValue](a0: (String, A0)): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue
    ](a0: (String, A0), a1: (String, A1)): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue
    ](a0: (String, A0), a1: (String, A1), a2: (String, A2)): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
        )
      )

    def queryString[A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue
    ](a0: (String, A0), a1: (String, A1), a2: (String, A2), a3: (String, A3)): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue,
      A4: HttpParamValue
    ](a0: (String, A0), a1: (String, A1), a2: (String, A2), a3: (String, A3), a4: (String, A4)): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
          a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
          a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue,
      A4: HttpParamValue,
      A5: HttpParamValue
    ]( a0: (String, A0),
       a1: (String, A1),
       a2: (String, A2),
       a3: (String, A3),
       a4: (String, A4),
       a5: (String, A5)
    ): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
          a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
          a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
          a5._1 -> implicitly[HttpParamValue[A5]].httpParamValue(a5._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue,
      A4: HttpParamValue,
      A5: HttpParamValue,
      A6: HttpParamValue,
    ]( a0: (String, A0),
       a1: (String, A1),
       a2: (String, A2),
       a3: (String, A3),
       a4: (String, A4),
       a5: (String, A5),
       a6: (String, A6)
    ): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
          a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
          a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
          a5._1 -> implicitly[HttpParamValue[A5]].httpParamValue(a5._2),
          a6._1 -> implicitly[HttpParamValue[A6]].httpParamValue(a6._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue,
      A4: HttpParamValue,
      A5: HttpParamValue,
      A6: HttpParamValue,
      A7: HttpParamValue,
    ](a0: (String, A0),
      a1: (String, A1),
      a2: (String, A2),
      a3: (String, A3),
      a4: (String, A4),
      a5: (String, A5),
      a6: (String, A6),
      a7: (String, A7),
     ): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
          a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
          a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
          a5._1 -> implicitly[HttpParamValue[A5]].httpParamValue(a5._2),
          a6._1 -> implicitly[HttpParamValue[A6]].httpParamValue(a6._2),
          a7._1 -> implicitly[HttpParamValue[A7]].httpParamValue(a7._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue,
      A4: HttpParamValue,
      A5: HttpParamValue,
      A6: HttpParamValue,
      A7: HttpParamValue,
      A8: HttpParamValue,
    ](a0: (String, A0),
      a1: (String, A1),
      a2: (String, A2),
      a3: (String, A3),
      a4: (String, A4),
      a5: (String, A5),
      a6: (String, A6),
      a7: (String, A7),
      a8: (String, A8),
     ): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
          a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
          a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
          a5._1 -> implicitly[HttpParamValue[A5]].httpParamValue(a5._2),
          a6._1 -> implicitly[HttpParamValue[A6]].httpParamValue(a6._2),
          a7._1 -> implicitly[HttpParamValue[A7]].httpParamValue(a7._2),
          a8._1 -> implicitly[HttpParamValue[A8]].httpParamValue(a8._2),
        )
      )

    def queryString[
      A0: HttpParamValue,
      A1: HttpParamValue,
      A2: HttpParamValue,
      A3: HttpParamValue,
      A4: HttpParamValue,
      A5: HttpParamValue,
      A6: HttpParamValue,
      A7: HttpParamValue,
      A8: HttpParamValue,
      A9: HttpParamValue,
    ](a0: (String, A0),
      a1: (String, A1),
      a2: (String, A2),
      a3: (String, A3),
      a4: (String, A4),
      a5: (String, A5),
      a6: (String, A6),
      a7: (String, A7),
      a8: (String, A8),
      a9: (String, A9)
     ): Option[String] =
      buildQueryStringOpt(
        List(
          a0._1 -> implicitly[HttpParamValue[A0]].httpParamValue(a0._2),
          a1._1 -> implicitly[HttpParamValue[A1]].httpParamValue(a1._2),
          a2._1 -> implicitly[HttpParamValue[A2]].httpParamValue(a2._2),
          a3._1 -> implicitly[HttpParamValue[A3]].httpParamValue(a3._2),
          a4._1 -> implicitly[HttpParamValue[A4]].httpParamValue(a4._2),
          a5._1 -> implicitly[HttpParamValue[A5]].httpParamValue(a5._2),
          a6._1 -> implicitly[HttpParamValue[A6]].httpParamValue(a6._2),
          a7._1 -> implicitly[HttpParamValue[A7]].httpParamValue(a7._2),
          a8._1 -> implicitly[HttpParamValue[A8]].httpParamValue(a8._2),
          a9._1 -> implicitly[HttpParamValue[A9]].httpParamValue(a9._2),
        )
      )
  }
}