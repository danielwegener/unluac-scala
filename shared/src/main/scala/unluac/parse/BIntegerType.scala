package unluac.parse

import java.math.BigInteger
import java.nio.ByteBuffer
import java.nio.ByteOrder

case class BIntegerType(intSize: Int) extends BObjectType[BInteger] {

  def raw_parse(buffer: ByteBuffer): BInteger = {
    var value: BInteger = null
    intSize match {
      case 0 =>
        value = new BInteger(0)
      case 1 =>
        value = new BInteger(buffer.get)
      case 2 =>
        value = new BInteger(buffer.getShort)
      case 4 =>
        value = new BInteger(buffer.getInt)
      case _ => {
        val bytes: Array[Byte] = new Array[Byte](intSize)
        var start: Int = 0
        var delta: Int = 1
        if (buffer.order eq ByteOrder.LITTLE_ENDIAN) {
          start = intSize - 1
          delta = -1
        }
        {
          var i: Int = start
          while (i >= 0 && i < intSize) {
            {
              bytes(i) = buffer.get
            }
            i += delta
          }
        }
        value = new BInteger(new BigInteger(bytes))
      }
    }
    value
  }

  def parse(buffer: ByteBuffer, header: BHeader): BInteger = {
    val value: BInteger = raw_parse(buffer)
    if (header.debug) {
      System.out.println("-- parsed <integer> " + value.asInt)
    }
    value
  }
}