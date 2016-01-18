package unluac

import java.io.{IOException, PrintStream, RandomAccessFile}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel

import unluac.decompile.{Decompiler, Output, OutputProvider}
import unluac.parse.{BHeader, LFunction}

import scala.scalajs.js.annotation.JSExport

@JSExport
object Main {
  var version: String = "1.2.2"


  def main(args: Array[String]):Unit =  {
    if (args.length == 0 || args.length > 1) {
      System.err.println("unluac v" + version)
      if (args.length == 0) {
        System.err.println("  error: no input file provided")
      }
      else {
        System.err.println("  error: too many arguments")
      }
      System.err.println("  usage: java -jar unluac.jar <file>")
      System.exit(1)
    }
    else {
      val fn: String = args(0)
      var lmain: LFunction = null
      try {
        lmain = file_to_function(fn)
      }
      catch {
        case e: IOException =>
          System.err.println("unluac v" + version)
          System.err.print("  error: ")
          System.err.println(e.getMessage)
          System.exit(1)
      }
      val d: Decompiler = new Decompiler(lmain)
      val block = d.decompile()
      d.print(block)
      System.exit(0)
    }
  }

  @throws(classOf[IOException])
  private def file_to_function(fn: String): LFunction = {
    val file: RandomAccessFile = new RandomAccessFile(fn, "r")
    val buffer: ByteBuffer = ByteBuffer.allocate(file.length.toInt)
    buffer.order(ByteOrder.LITTLE_ENDIAN)
    var len: Int = file.length.toInt
    val in: FileChannel = file.getChannel
    while (len > 0) len -= in.read(buffer)
    buffer.rewind
    val header: BHeader = BHeader(buffer)
    header.function.parse(buffer, header)
  }

  @throws(classOf[IOException])
  def decompile(in: String, out: String) {
    val lmain: LFunction = file_to_function(in)
    val d: Decompiler = new Decompiler(lmain)
    val block = d.decompile()
    val pout: PrintStream = new PrintStream(out)
    d.print(new Output(new OutputProvider() {
      def print(s: String) {
        pout.print(s)
      }

      def println() {
        pout.println()
      }
    }), block)
    pout.flush()
    pout.close()
  }
}