package xyz.cofe.lima.docker.model

import tethys.JsonReader
import tethys.derivation.semiauto.jsonReader
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class ContainerConfig(
                            Image: String,
                            AttachStdin: Boolean,
                            AttachStdout: Boolean,
                            AttachStderr: Boolean,
                            Tty: Boolean,
                            OpenStdin: Boolean,
                            StdinOnce: Boolean,
                            Hostname: Option[String],
                            Domainname: Option[String],
                            User: Option[String],
                            ExposedPorts: Map[String,Map[String,String]]=Map(),
                            Env: Option[List[String]]=None,
                            Cmd: Option[List[String]]=None,
                            Healthcheck: Option[HealthConfig]=None,
                            ArgsEscaped: Option[Boolean]=None,
                            Volumes: Map[String,Map[String,String]]=Map(),
                            WorkingDir: Option[String]=None,
                            Entrypoint: Option[List[String]]=None,
                            NetworkDisabled: Option[Boolean]=None,
                            MacAddress: Option[String]=None,
                            OnBuild: Option[List[String]]=None,
                            Labels: Map[String,String]=Map(),
                            StopSignal: Option[String]=None,
                            StopTimeout: Option[String]=None,
                            Shell: Option[List[String]]=None,
                          )
object ContainerConfig {
  implicit val reader: JsonReader[ContainerConfig] = jsonReader[ContainerConfig]
}
