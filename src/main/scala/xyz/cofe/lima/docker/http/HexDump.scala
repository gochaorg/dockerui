package xyz.cofe.lima.docker.http

object HexDump {
  def bytesFrom(string: String) = {
    string.split("\\r?\\n")
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap(_.split("\\s+"))
      .map { bstr =>
        if( bstr.length!=2 ){
          throw new Error
        }else{
          Integer.parseInt(bstr.substring(0,1) + bstr.substring(1,2), 16).toByte
        }
      }
  }

  val digits = "0123456789abcdef"
  def toString(bytes:Seq[Byte], prefix:String="", groupSize:Int=32) = {
    bytes.grouped(groupSize).map { block =>
      prefix + block.map { b =>
        val lo = b & 0x0f
        val hi = (b & 0xf0) >> 4
        digits.charAt(hi).toString + digits.charAt(lo).toString
      }.mkString(" ")
    }.mkString("\n")
  }
}
