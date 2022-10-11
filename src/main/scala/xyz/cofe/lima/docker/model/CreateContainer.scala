package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.jsonReader
import tethys.derivation.semiauto.jsonWriter
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class CreateContainerRequest(
                            Hostname: Option[String],
                            Domainname: Option[String],
                            User: Option[String],
                            AttachStdin:Option[Boolean],
                            AttachStdout:Option[Boolean],
                            AttachStderr:Option[Boolean],

                            /**
                             * An object mapping ports to an empty object in the form:
                             *
                             * <pre>{"&lt;port&gt;/&lt;tcp|udp|sctp&gt;": {}}</pre>
                             * <pre>"22/tcp": { }</pre>
                             */
                            ExposedPorts: Option[Map[String,Map[String,String]]],

                            Tty: Option[Boolean],
                            OpenStdin: Option[Boolean],
                            StdinOnce: Option[Boolean],
                            Env: Option[List[String]],
                            Cmd: Option[List[String]],
                            HealthConfig: Option[HealthConfig],
                            ArgsEscaped: Option[Boolean],
                            Image: String,
                            // Volumes { "/volumes/data": { } }
                            WorkingDir: Option[String],
                            Entrypoint: Option[List[String]],
                            NetworkDisabled: Option[Boolean],
                            MacAddress: Option[Boolean],
                            OnBuild: Option[List[String]],
                            Labels: Map[String,String],
                            StopSignal: Option[String],
                            StopTimeout: Option[Int],
                            Shell: Option[String],
                            HostConfig: Option[HostConfig],
                            //NetworkingConfig: NetworkingConfig
                          )
object CreateContainerRequest {
  implicit val reader: JsonReader[CreateContainerRequest] = jsonReader[CreateContainerRequest]
  implicit val writer: JsonWriter[CreateContainerRequest] = jsonWriter[CreateContainerRequest]
}


