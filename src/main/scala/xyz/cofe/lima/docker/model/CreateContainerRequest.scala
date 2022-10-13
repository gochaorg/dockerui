package xyz.cofe.lima.docker.model

import tethys.{JsonReader, JsonWriter}
import tethys.derivation.semiauto.jsonReader
import tethys.derivation.semiauto.jsonWriter
import xyz.cofe.lima.{Show, ShowDerivation, TreeShow, TreeShowDerivation}

case class CreateContainerRequest(
                            Image: String,
                            Hostname: Option[String]=None,
                            Domainname: Option[String]=None,
                            User: Option[String]=None,
                            AttachStdin:Option[Boolean]=None,
                            AttachStdout:Option[Boolean]=None,
                            AttachStderr:Option[Boolean]=None,

                            /**
                             * An object mapping ports to an empty object in the form:
                             *
                             * <pre>{"&lt;port&gt;/&lt;tcp|udp|sctp&gt;": {}}</pre>
                             * <pre>"22/tcp": { }</pre>
                             */
                            ExposedPorts: Option[Map[String,Map[String,String]]]=None,

                            Tty: Option[Boolean]=None,
                            OpenStdin: Option[Boolean]=None,
                            StdinOnce: Option[Boolean]=None,
                            Env: Option[List[String]]=None,
                            Cmd: Option[List[String]]=None,
                            HealthConfig: Option[HealthConfig]=None,
                            ArgsEscaped: Option[Boolean]=None,
                            // Volumes { "/volumes/data": { } }
                            WorkingDir: Option[String]=None,
                            Entrypoint: Option[List[String]]=None,
                            NetworkDisabled: Option[Boolean]=None,
                            MacAddress: Option[Boolean]=None,
                            OnBuild: Option[List[String]]=None,
                            Labels: Map[String,String]=Map(),
                            StopSignal: Option[String]=None,
                            StopTimeout: Option[Int]=None,
                            Shell: Option[String]=None,
                            HostConfig: Option[HostConfig]=None,
                            //NetworkingConfig: NetworkingConfig
                          )
object CreateContainerRequest {
  implicit val reader: JsonReader[CreateContainerRequest] = jsonReader[CreateContainerRequest]
  implicit val writer: JsonWriter[CreateContainerRequest] = jsonWriter[CreateContainerRequest]
}


