package unluac

import unluac.decompile.{Decompiler, OutputProvider}
import unluac.parse.BHeader

import scala.scalajs.js.annotation.JSExport
import scala.scalajs.js.typedarray.{ArrayBuffer, TypedArrayBuffer}

@JSExport
object UnLuac {

  private class StringOutput extends OutputProvider {
    val sb: StringBuilder = new StringBuilder

    override def print(s: String): Unit = sb.append(s)

    override def println(): Unit = sb.append("\n")
  }

  @JSExport
  def decompile(arrayBuffer: ArrayBuffer): String = {
    val buffer          = TypedArrayBuffer.wrap(arrayBuffer)
    val header: BHeader = BHeader(buffer)
    val lFunction       = header.function.parse(buffer, header)
    val d: Decompiler   = new Decompiler(lFunction)
    val block           = d.decompile()
    val outputProvider  = new StringOutput
    d.print(outputProvider, block)
    outputProvider.sb.toString()
  }
}
