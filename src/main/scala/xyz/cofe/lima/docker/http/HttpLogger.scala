package xyz.cofe.lima.docker.http

trait HttpLogger {
  def send(httpRequest: HttpRequest)
  def receive(httpResponse: HttpResponse)
}

object HttpLogger {
  implicit val defaultLogger = new HttpLogger {
    override def send(httpRequest: HttpRequest): Unit = {}
    override def receive(httpResponse: HttpResponse): Unit = {}
  }
  def stdout:HttpLogger = new HttpLogger {
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
  }
}
