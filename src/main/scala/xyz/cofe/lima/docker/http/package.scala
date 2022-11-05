package xyz.cofe.lima.docker

import tethys.JsonReader
import tethys._
import tethys.jackson._

import scala.reflect.ClassTag

package object http {
  implicit class HttpResponseOps(val response: Either[errors.HttpError, HttpResponse]) {
    def validStatusCode(code: Int*): Either[errors.HttpError, HttpResponse] = {
      response.flatMap { resp =>
        resp.code match {
          case Some(statusCode) =>
            if (code.contains(statusCode)) {
              Right(resp)
            } else {
              Left(errors.HttpError.HttpStatusCodeNotValid(s"code=$statusCode, but expected is ${code}"))
            }
          case None => Left(errors.HttpError.HttpStatusCodeNotValid("code undefined"))
        }
      }
    }

    def text:Either[errors.HttpError, String] = {
      response.flatMap { resp =>
        resp.text match {
          case Some(value) => Right(value)
          case None => Left(errors.HttpError.CantExtractText())
        }
      }
    }

    def json[A: JsonReader](implicit ct: ClassTag[A]):Either[errors.HttpError, A] = {
      text.flatMap { bodyText =>
        val cname = ct.runtimeClass.toGenericString
        bodyText.jsonAs[A].left.map { readerErr =>
          errors.HttpError.CantExtractJson(bodyText, cname, readerErr)
        }
      }
    }
  }
}
