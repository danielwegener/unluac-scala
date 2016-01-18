package unluac.parse

import java.nio.ByteBuffer

case class BSizeTType(sizeTSize: Int) extends BObjectType[BSizeT] {
  private val integerType: BIntegerType = new BIntegerType(sizeTSize)

  def parse(buffer: ByteBuffer, header: BHeader): BSizeT = {
    val value: BSizeT = new BSizeT(integerType.raw_parse(buffer))
    if (header.debug) {
      System.out.println("-- parsed <size_t> " + value.asInt)
    }
    value
  }
}