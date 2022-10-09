package xyz.cofe.lima.docker.http

case class SingleSupplier[O]( var items:Seq[O] ) extends Function0[Option[O]] {
  def apply():Option[O] = {
    val res = items.headOption
    items = if( items.nonEmpty ) items.tail else items
    res
  }
}

object SingleSupplier {
  def fromHexDump(string:String):SingleSupplier[Byte] =
    SingleSupplier(
      string
        .split("\\r?\\n")
        .map(_.trim)
        .filter(_.nonEmpty)
        .flatMap(_.split("\\s+"))
        .map { bstr =>
          if( bstr.length!=2 ){
            throw new Error
          }else{
            Integer.parseInt(bstr.substring(0,1) + bstr.substring(1,2), 16).toByte
          }
        }.toList
    )
}