package unluac.parse

import java.nio.ByteBuffer

class LLocalType extends BObjectType[LLocal] {
  def parse(buffer: ByteBuffer, header: BHeader): LLocal = {
    val name: LString = header.string.parse(buffer, header)
    val start: BInteger = header.integer.parse(buffer, header)
    val end: BInteger = header.integer.parse(buffer, header)
    if (header.debug) {
      System.out.print("-- parsing local, name: ")
      System.out.print(name)
      System.out.print(" from " + start.asInt + " to " + end.asInt)
      System.out.println()
    }
    new LLocal(name, start, end)
  }
}