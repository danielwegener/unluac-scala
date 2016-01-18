package unluac.parse

import java.nio.ByteBuffer

class LFunctionType52 extends LFunctionType {
  protected override def parse_main(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    s.lineBegin = header.integer.parse(buffer, header).asInt
    s.lineEnd = header.integer.parse(buffer, header).asInt
    s.lenParameter = 0xFF & buffer.get
    s.vararg = 0xFF & buffer.get
    s.maximumStackSize = 0xFF & buffer.get
    parse_code(buffer, header, s)
    parse_constants(buffer, header, s)
    parse_upvalues(buffer, header, s)
    parse_debug(buffer, header, s)
  }

  protected override def parse_debug(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    s.name = header.string.parse(buffer, header)
    super.parse_debug(buffer, header, s)
  }

  protected override def parse_upvalues(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    val upvalues: BList[LUpvalue] = header.upvalue.parseList(buffer, header)
    s.lenUpvalues = upvalues.length.asInt
    s.upvalues = upvalues.asArray(new Array[LUpvalue](s.lenUpvalues))
  }
}