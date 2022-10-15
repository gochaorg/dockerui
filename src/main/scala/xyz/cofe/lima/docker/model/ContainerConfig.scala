package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.{jsonReader, jsonWriter}

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
                            ExposedPorts: Option[Map[String,Map[String,String]]]=None,
                            Env: Option[List[String]]=None,
                            Cmd: Option[List[String]]=None,
                            Healthcheck: Option[HealthConfig]=None,
                            ArgsEscaped: Option[Boolean]=None,
                            Volumes: Option[Map[String,Map[String,String]]]=None,
                            WorkingDir: Option[String]=None,
                            Entrypoint: Option[List[String]]=None,
                            NetworkDisabled: Option[Boolean]=None,
                            MacAddress: Option[String]=None,
                            OnBuild: Option[List[String]]=None,
                            Labels: Option[Map[String,String]]=None,
                            StopSignal: Option[String]=None,
                            StopTimeout: Option[String]=None,
                            Shell: Option[List[String]]=None,
                          )
object ContainerConfig {
  implicit val reader: JsonReader[ContainerConfig] = jsonReader[ContainerConfig]
  implicit val writer: JsonWriter[ContainerConfig] = jsonWriter[ContainerConfig]
}
