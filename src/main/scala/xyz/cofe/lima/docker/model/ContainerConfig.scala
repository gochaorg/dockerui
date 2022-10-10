package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class ContainerConfig(
                            Hostname: String,
                            Domainname: String,
                            User: String,
                            AttachStdin: Boolean,
                            AttachStdout: Boolean,
                            AttachStderr: Boolean,
                            // ExposedPorts
                            Tty: Boolean,
                            OpenStdin: Boolean,
                            StdinOnce: Boolean,
                            Env: List[String],
                            Cmd: List[String],
                            // Healthcheck
                            ArgsEscaped: Option[Boolean],
                            Image: String,
                            // Volumes
                            WorkingDir: String,
                            Entrypoint: Option[List[String]],
                            NetworkDisabled: Option[Boolean],
                            MacAddress: Option[String],
                            OnBuild: Option[List[String]],
                            // Labels:
                            StopSignal: Option[String],
                            StopTimeout: Option[String],
                            Shell: Option[List[String]],
                          )
object ContainerConfig {
  implicit val reader: JsonReader[ContainerConfig] = jsonReader[ContainerConfig]
}
