package unluac.parse

import java.nio.ByteBuffer

class LConstantType extends BObjectType[LObject] {

  def parse(buffer: ByteBuffer, header: BHeader): LObject = {
    val `type`: Int = 0xFF & buffer.get
    if (header.debug) {
      System.out.print("-- parsing <constant>, type is ")
      `type` match {
        case 0 =>
          System.out.println("<nil>")
        case 1 =>
          System.out.println("<boolean>")
        case 3 =>
          System.out.println("<number>")
        case 4 =>
          System.out.println("<strin>")
        case _ =>
          System.out.println("illegal " + `type`)
      }
    }
    `type` match {
      case 0 =>
        LNil.NIL
      case 1 =>
        header.bool.parse(buffer, header)
      case 3 =>
        header.number.parse(buffer, header)
      case 4 =>
        header.string.parse(buffer, header)
      case _ =>
        throw new IllegalStateException
    }
  }
}