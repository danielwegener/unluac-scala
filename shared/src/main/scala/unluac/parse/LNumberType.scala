package unluac.parse

import java.nio.ByteBuffer

case class LNumberType(size:Int, integral:Boolean) extends BObjectType[LNumber] {

  if (!(size == 4 || size == 8)) {
    throw new IllegalStateException("The input chunk has an unsupported Lua number size: " + size)
  }

  def parse(buffer: ByteBuffer, header: BHeader): LNumber = {
    var value: LNumber = null
    if (integral) {
      size match {
        case 4 =>
          value = new LIntNumber(buffer.getInt)
        case 8 =>
          value = new LLongNumber(buffer.getLong)
      }
    }
    else {
      size match {
        case 4 =>
          value = new LFloatNumber(buffer.getFloat)
        case 8 =>
          value = new LDoubleNumber(buffer.getDouble)
      }
    }
    if (value == null) {
      throw new IllegalStateException("The input chunk has an unsupported Lua number format")
    }
    if (header.debug) {
      System.out.println("-- parsed <number> " + value)
    }
    value
  }
}