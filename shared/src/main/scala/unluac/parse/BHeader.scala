package unluac.parse

import java.nio.ByteBuffer
import java.nio.ByteOrder
import unluac.Version

object BHeader {
  private val signature: Array[Byte] = Array[Byte](0x1B, 0x4C, 0x75, 0x61)
  private val luacTail: Array[Byte] = Array[Byte](0x19, 0x93.toByte, 0x0D, 0x0A, 0x1A, 0x0A)


  def apply(buffer: ByteBuffer):BHeader = {
    val debug = false
    var i: Int = 0
    while (i < BHeader.signature.length) {
      if (buffer.get != BHeader.signature(i)) {
        throw new IllegalStateException("The input file does not have the signature of a valid Lua file.")
      }
      i += 1
    }
    val versionNumber: Int = 0xFF & buffer.get
    val version = versionNumber match {
      case 0x51 =>Version.LUA51
      case 0x52 =>Version.LUA52
      case _ =>
        val major: Int = versionNumber >> 4
        val minor: Int = versionNumber & 0x0F
        throw new IllegalStateException("The input chunk's Lua version is " + major + "." + minor + "; unluac can only handle Lua 5.1 and Lua 5.2.")

    }
    if (debug) {
      System.out.println("-- version: 0x" + Integer.toHexString(versionNumber))
    }
    val format: Int = 0xFF & buffer.get
    if (format != 0) {
      throw new IllegalStateException("The input chunk reports a non-standard lua format: " + format)
    }
    if (debug) {
      System.out.println("-- format: " + format)
    }
    val endianness: Int = 0xFF & buffer.get
    endianness match {
      case 0 =>
        buffer.order(ByteOrder.BIG_ENDIAN)
      case 1 =>
        buffer.order(ByteOrder.LITTLE_ENDIAN)
      case _ =>
        throw new IllegalStateException("The input chunk reports an invalid endianness: " + endianness)
    }
    if (debug) {
      System.out.println("-- endianness: " + endianness + (if (endianness == 0) " (big)" else " (little)"))
    }
    val intSize: Int = 0xFF & buffer.get
    if (debug) {
      System.out.println("-- int size: " + intSize)
    }
    val integer = new BIntegerType(intSize)
    val sizeTSize: Int = 0xFF & buffer.get
    if (debug) {
      System.out.println("-- size_t size: " + sizeTSize)
    }
    val sizeT = new BSizeTType(sizeTSize)
    val instructionSize: Int = 0xFF & buffer.get
    if (debug) {
      System.out.println("-- instruction size: " + instructionSize)
    }
    if (instructionSize != 4) {
      throw new IllegalStateException("The input chunk reports an unsupported instruction size: " + instructionSize + " bytes")
    }
    val lNumberSize: Int = 0xFF & buffer.get
    if (debug) {
      System.out.println("-- Lua number size: " + lNumberSize)
    }
    val lNumberIntegralCode: Int = 0xFF & buffer.get
    if (debug) {
      System.out.println("-- Lua number integral code: " + lNumberIntegralCode)
    }
    if (lNumberIntegralCode > 1) {
      throw new IllegalStateException("The input chunk reports an invalid code for lua number integralness: " + lNumberIntegralCode)
    }
    val lNumberIntegral: Boolean = lNumberIntegralCode == 1
    val number = new LNumberType(lNumberSize, lNumberIntegral)
    val bool = new LBooleanType
    val string = new LStringType
    val constant = new LConstantType
    val local = new LLocalType
    val upvalue = new LUpvalueType
    val function = version.getLFunctionType
    if (version.hasHeaderTail) {
      {
        var i: Int = 0
        while (i < BHeader.luacTail.length) {
          {
            if (buffer.get != BHeader.luacTail(i)) {
              throw new IllegalStateException("The input file does not have the header tail of a valid Lua file.")
            }
            i += 1
          }
        }
      }
    }

    BHeader(debug,
      version,
      integer,
      sizeT,
      bool,
      number,
      string,
      constant,
      local,
      upvalue,
      function)
  }

}

case class BHeader(
debug: Boolean ,
version: Version ,
integer: BIntegerType ,
sizeT: BSizeTType ,
bool: LBooleanType ,
number: LNumberType ,
string: LStringType ,
constant: LConstantType ,
local: LLocalType ,
upvalue: LUpvalueType ,
function: LFunctionType
             )