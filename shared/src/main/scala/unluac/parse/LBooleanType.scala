package unluac.parse

import java.nio.ByteBuffer

class LBooleanType extends BObjectType[LBoolean] {

  def parse(buffer: ByteBuffer, header: BHeader): LBoolean = {
    val value: Int = buffer.get
    if ((value & 0xFFFFFFFE) != 0) {
      throw new IllegalStateException
    }
    else {
      val bool: LBoolean = if (value == 0) LBoolean.LFALSE else LBoolean.LTRUE
      if (header.debug) {
        System.out.println("-- parsed <boolean> " + bool)
      }
      bool
    }
  }
}