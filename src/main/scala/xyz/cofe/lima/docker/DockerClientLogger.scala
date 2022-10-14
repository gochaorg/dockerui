package xyz.cofe.lima.docker

trait DockerClientLogger {

}

object DockerClientLogger {
  implicit val defaultLogger = new DockerClientLogger {}
}
