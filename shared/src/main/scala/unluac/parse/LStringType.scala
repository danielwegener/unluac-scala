package unluac.parse

import java.nio.ByteBuffer

class LStringType extends BObjectType[LString] {

  def parse(buffer: ByteBuffer, header: BHeader): LString = {
    val sizeT: BSizeT = header.sizeT.parse(buffer, header)
    val b: StringBuilder = new StringBuilder()
    sizeT.iterate(new Runnable() {
      def run() {
        b.append((0xFF & buffer.get).toChar)
      }
    })
    val s: String = b.toString
    if (header.debug) {
      System.out.println("-- parsed <string> \"" + s + "\"")
    }
    new LString(sizeT, s)
  }
}