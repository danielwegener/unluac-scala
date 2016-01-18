package unluac.parse

import java.nio.ByteBuffer

object LFunctionType {
  val TYPE51: LFunctionType = new LFunctionType
  val TYPE52: LFunctionType = new LFunctionType52
}

class LFunctionType extends BObjectType[LFunction] {
  def parse(buffer: ByteBuffer, header: BHeader): LFunction = {
    if (header.debug) {
      System.out.println("-- beginning to parse function")
    }
    if (header.debug) {
      System.out.println("-- parsing name...start...end...upvalues...params...varargs...stack")
    }
    val s: LFunctionParseState = new LFunctionParseState
    parse_main(buffer, header, s)
    new LFunction(header, s.code, s.locals.asArray(new Array[LLocal](s.locals.length.asInt)), s.constants.asArray(new Array[LObject](s.constants.length.asInt)), s.upvalues, s.functions.asArray(new Array[LFunction](s.functions.length.asInt)), s.maximumStackSize, s.lenUpvalues, s.lenParameter, s.vararg)
  }

  protected def parse_main(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    s.name = header.string.parse(buffer, header)
    s.lineBegin = header.integer.parse(buffer, header).asInt
    s.lineEnd = header.integer.parse(buffer, header).asInt
    s.lenUpvalues = 0xFF & buffer.get
    s.lenParameter = 0xFF & buffer.get
    s.vararg = 0xFF & buffer.get
    s.maximumStackSize = 0xFF & buffer.get
    parse_code(buffer, header, s)
    parse_constants(buffer, header, s)
    parse_upvalues(buffer, header, s)
    parse_debug(buffer, header, s)
  }

  protected def parse_code(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    if (header.debug) {
      System.out.println("-- beginning to parse bytecode list")
    }
    s.length = header.integer.parse(buffer, header).asInt
    s.code = new Array[Int](s.length)

      var i: Int = 0
      while (i < s.length) {
          s.code(i) = buffer.getInt
          if (header.debug) {
            System.out.println("-- parsed codepoint " + Integer.toHexString(s.code(i)))
          }
          i += 1
      }

  }

  protected def parse_constants(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState): Unit = {
    if (header.debug) {
      System.out.println("-- beginning to parse constants list")
    }
    s.constants = header.constant.parseList(buffer, header)
    if (header.debug) {
      System.out.println("-- beginning to parse functions list")
    }
    s.functions = header.function.parseList(buffer, header)
  }

  protected def parse_debug(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    if (header.debug) {
      System.out.println("-- beginning to parse source lines list")
    }
    s.lines = header.integer.parseList(buffer, header)
    if (header.debug) {
      System.out.println("-- beginning to parse locals list")
    }
    s.locals = header.local.parseList(buffer, header)
    if (header.debug) {
      System.out.println("-- beginning to parse upvalues list")
    }
    val upvalueNames: BList[LString] = header.string.parseList(buffer, header)

      var i: Int = 0
      while (i < upvalueNames.length.asInt) {
          s.upvalues(i).name = upvalueNames.get(i).deref
          i += 1
      }

  }

  protected def parse_upvalues(buffer: ByteBuffer, header: BHeader, s: LFunctionParseState) {
    s.upvalues = new Array[LUpvalue](s.lenUpvalues)
      var i: Int = 0
      while (i < s.lenUpvalues) {
          s.upvalues(i) = new LUpvalue
          i += 1
      }
  }
}