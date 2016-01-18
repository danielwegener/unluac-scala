package unluac.parse

import java.nio.ByteBuffer

class LUpvalueType extends BObjectType[LUpvalue] {

  def parse(buffer: ByteBuffer, header: BHeader): LUpvalue = {
    val upvalue: LUpvalue = new LUpvalue
    upvalue.instack = buffer.get != 0
    upvalue.idx = 0xFF & buffer.get
    upvalue
  }

}