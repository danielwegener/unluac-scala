package unluac.decompile

import unluac.parse.LFunction

object Code {
  def extract_A(codepoint: Int): Int = {
    (codepoint >> 6) & 0x0000000FF
  }

  def extract_C(codepoint: Int): Int = {
    (codepoint >> 14) & 0x000001FF
  }

  def extract_B(codepoint: Int): Int = {
    codepoint >>> 23
  }

  def extract_Bx(codepoint: Int): Int = {
    codepoint >>> 14
  }

  def extract_sBx(codepoint: Int): Int = {
    (codepoint >>> 14) - 131071
  }
}

case class Code(map:OpcodeMap, code:Array[Int]) {

  def this(function: LFunction) {
    this(function.header.version.getOpcodeMap, function.code)
  }

  def op(line: Int): Op = {
    map.get(code(line - 1) & 0x0000003F)
  }

  def A(line: Int): Int = {
    Code.extract_A(code(line - 1))
  }

  def C(line: Int): Int = {
    Code.extract_C(code(line - 1))
  }

  def B(line: Int): Int = {
    Code.extract_B(code(line - 1))
  }

  def Bx(line: Int): Int = {
    Code.extract_Bx(code(line - 1))
  }

  def sBx(line: Int): Int = {
    Code.extract_sBx(code(line - 1))
  }

  def codepoint(line: Int): Int = {
    code(line - 1)
  }

  def toString(line: Int): String = {
    op(line).codePointToString(codepoint(line))
  }
}