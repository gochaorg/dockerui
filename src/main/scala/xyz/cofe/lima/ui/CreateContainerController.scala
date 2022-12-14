package xyz.cofe.lima.ui

import javafx.application.Platform
import javafx.fxml.FXML
import javafx.scene.control
import javafx.scene.control.{TreeItem, TreeTableView}
import xyz.cofe.lima.docker.DockerClient
import xyz.cofe.lima.docker.log.Logger.ContainerCreate
import xyz.cofe.lima.docker.model.{CreateContainerRequest, HostConfig}
import xyz.cofe.lima.store.{ControllersHistory, History}

/**
 * Запуск контейнера
 */
//noinspection SimplifyBooleanMatch,ConvertNullInitializerToUnderscore
class CreateContainerController {
  @FXML
  private var params : TreeTableView[MutProp] = null

  @FXML
  def initialize():Unit = {
    MutProp.initPropTree(params)

    history.last.foreach { h =>
      request = h.createContainerRequest
      name = h.name
      platform = h.platform

      historyIndex = Some(history.size-1)
    }
  }

  var request:CreateContainerRequest = CreateContainerRequest("image name")
  var name:Option[String] = None
  var platform:Option[String] = None

  def history: History[ContainerCreate] = ControllersHistory.createContainerHistory
  var historyIndex:Option[Int] = None
  def historyPrev():Unit = {
    historyIndex match {
      case Some(idx) if idx > 0 =>
        history.get(idx-1).foreach { h =>
          request = h.createContainerRequest
          name = h.name
          platform = h.platform
          historyIndex = Some(idx-1)
          params.refresh()
        }
      case _ =>
    }
  }
  def historyNext():Unit = {
    historyIndex match {
      case Some(idx) if idx < (history.size-1) =>
        history.get(idx+1).foreach { h =>
          request = h.createContainerRequest
          name = h.name
          platform = h.platform
          historyIndex = Some(idx+1)
          params.refresh()
        }
      case _ =>
    }
  }

  def prepareEdit():Unit = {
    val root = new TreeItem(MutProp("CreateContainerController",()=>"",_=>()))

    val containerName = new TreeItem(MutProp("name",() => name.getOrElse(""), { v => name = if(v.trim.length<1) None else Some(v) }))
    root.getChildren.add(containerName)

    val platformName = new TreeItem(MutProp("platform",() => platform.getOrElse(""), { v => platform = if(v.trim.length<1) None else Some(v) }))
    root.getChildren.add(platformName)

    val imageName = new TreeItem(MutProp("image",() => request.Image, { v => request = request.copy(Image = v) }))
    root.getChildren.add(imageName)

    val userName = new TreeItem(MutProp("user",() => request.User.getOrElse(""), { v => request = request.copy(User = Some(v)) }))
    root.getChildren.add(userName)

    val attachStdin = new TreeItem(MutProp("attachStdin:Boolean",
      () => request.AttachStdin.map(_.toString).getOrElse(""),
      v => request = request.copy(AttachStdin = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(attachStdin)

    val attachStdout = new TreeItem(MutProp("attachStdout:Boolean",
      () => request.AttachStdout.map(_.toString).getOrElse(""),
      v => request = request.copy(AttachStdout = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(attachStdout)

    val attachStderr = new TreeItem(MutProp("attachStderr:Boolean",
      () => request.AttachStderr.map(_.toString).getOrElse(""),
      v => request = request.copy(AttachStderr = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(attachStderr)

    val tty = new TreeItem(MutProp("tty:Boolean",
      () => request.Tty.map(_.toString).getOrElse(""),
      v => request = request.copy(Tty = v.trim.length<1 match {
        case true => None
        case false => Some(v.matches("(?is)true|1|on"))
      })
    ))
    root.getChildren.add(tty)

    val cmd = new TreeItem(MutProp("cmd",
      () => request.Tty.map(_.toString).getOrElse(""),
      _ => request = request))
    root.getChildren.add(cmd)

    def rebuildCmdRoot():Unit = {
      cmd.getChildren.clear()
      request.Cmd.foreach { cmdList =>
        cmdList.zipWithIndex.foreach { case (c, i) =>
          val ci = new TreeItem(MutProp(s"#$i",
            () => request.Cmd.map(cmdl => cmdl(i)).getOrElse(""),
            v => {
              request = request.copy(
                Cmd = if( v.nonEmpty ) {
                  request.Cmd.map(eCmdLine => eCmdLine.updated(i, v))
                }else{
                  request.Cmd.map(eCmdLine => eCmdLine.zipWithIndex.filter { case(_,ii) => ii!=i }.map(_._1))
                }
              )
              Platform.runLater(()=>{
                rebuildCmdRoot()
              })
            }
          ))
          cmd.getChildren.add(ci)
        }
      }
      val ci = new TreeItem(MutProp("new",()=>"",
        v => {
          if(v.trim.nonEmpty){
            request = request.copy(
              Cmd = request.Cmd.map(eCmdLine => eCmdLine ++ List(v)).orElse(Some(List(v)))
            )
            Platform.runLater(()=>{
              rebuildCmdRoot()
            })
          }
        }
      ))
      cmd.getChildren.add(ci)
    }
    rebuildCmdRoot()

    val hostConfigNode = new TreeItem(MutProp("hostConfig", ()=>"", _ => ()));
    root.getChildren.add(hostConfigNode)

    def rebuildHostConfig():Unit = {
      val bindsNode = new TreeItem(MutProp("Binds",()=>"",v=>()))
      hostConfigNode.getChildren.add(bindsNode)

      def rebuildBinds():Unit = {
        bindsNode.getChildren.clear()

        val existsBinds = request.HostConfig.flatMap(_.Binds).getOrElse(List())

        request.HostConfig.foreach { _.Binds.foreach { bindsList =>
          bindsList.zipWithIndex.foreach { case (bindValue,idx) =>
            val bindNode = new TreeItem(MutProp(s"#$idx",()=>bindValue,v => {
              val newBinds =
                if( v.isEmpty ){
                  existsBinds.zipWithIndex.filter{case(_,i)=>i!=idx}.map(_._1)
                }else{
                  existsBinds.updated(idx, v)
                }

              request = request.copy(
                HostConfig =
                  if (newBinds.isEmpty)
                    None
                  else
                    Some(
                      request.HostConfig.getOrElse(new HostConfig()).copy(
                        Binds = Some(newBinds)
                      )
                    )
              )

              Platform.runLater(()=>rebuildBinds())
            }))
            bindsNode.getChildren.add(bindNode)
          }
        }}

        val newBindMnt = new TreeItem(MutProp("new",()=>"src:dest",appendStr=>{
          val existsBinds = request.HostConfig.flatMap(_.Binds).getOrElse(List())
          val newBinds = existsBinds :+ appendStr
          request = request.copy(
            HostConfig = Some(
              request.HostConfig.getOrElse(new HostConfig()).copy(
                Binds = Some(newBinds)
              )
            )
          )
          Platform.runLater(()=>rebuildBinds())
        }))

        bindsNode.getChildren.add(newBindMnt)
      }
      rebuildBinds()
    }
    rebuildHostConfig()

    params.setShowRoot(false)
    params.setRoot(root)
  }

  def createContainer():Unit = {
    request = request.copy(
      Cmd = request.Cmd.flatMap { cmdList =>
        if( cmdList.isEmpty ){
          None
        }else{
          Some(cmdList)
        }
      }
    )
    history.add(ContainerCreate(request, name, platform))
    DockerClientPool.submit { dc =>
      dc.containerCreate(request, name, platform) match {
        case Left(err) =>
        case Right(resp) => println(resp)
      }
    }
  }
}
