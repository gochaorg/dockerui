package xyz.cofe.lima.docker

import xyz.cofe.lima.errors.AppError

object errors {
  sealed trait DockerError extends AppError

}
