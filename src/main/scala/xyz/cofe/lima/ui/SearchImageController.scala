package xyz.cofe.lima.ui

import xyz.cofe.lima.docker.model
import xyz.cofe.lima.docker.hub.{model => hmodel}

/** Поиск образов на docker hub */
class SearchImageController {
}

object SearchImageController {
  sealed trait SearchNode
  case class SearchImageResp( searchImage:model.ImageSearch )
  case class SearchTag( tag:hmodel.Tag )


}