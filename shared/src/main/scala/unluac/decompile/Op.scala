package unluac.decompile

sealed trait Op extends Product {

  def format:OpcodeFormat

  def codePointToString(codepoint: Int): String = {
    format match {
      case OpcodeFormat.A =>
        productPrefix + " " + Code.extract_A(codepoint)
      case OpcodeFormat.A_B =>
        productPrefix + " " + Code.extract_A(codepoint) + " " + Code.extract_B(codepoint)
      case OpcodeFormat.A_C =>
        productPrefix + " " + Code.extract_A(codepoint) + " " + Code.extract_C(codepoint)
      case OpcodeFormat.A_B_C =>
        productPrefix + " " + Code.extract_A(codepoint) + " " + Code.extract_B(codepoint) + " " + Code.extract_C(codepoint)
      case OpcodeFormat.A_Bx =>
        productPrefix + " " + Code.extract_A(codepoint) + " " + Code.extract_Bx(codepoint)
      case OpcodeFormat.A_sBx =>
        productPrefix + " " + Code.extract_A(codepoint) + " " + Code.extract_sBx(codepoint)
      case OpcodeFormat.Ax =>
        productPrefix + " <Ax>"
      case OpcodeFormat.sBx =>
        productPrefix + " " + Code.extract_sBx(codepoint)
      case _ =>
        productPrefix
    }
  }
}

object Op {

  abstract class OpImpl private[Op](val format:OpcodeFormat) extends Op

  case object MOVE extends OpImpl(OpcodeFormat.A_B)
  case object LOADK  extends OpImpl (OpcodeFormat.A_Bx)
  case object LOADBOOL  extends OpImpl (OpcodeFormat.A_B_C)
  case object LOADNIL  extends OpImpl (OpcodeFormat.A_B)
  case object GETUPVAL  extends OpImpl (OpcodeFormat.A_B)
  case object GETGLOBAL  extends OpImpl (OpcodeFormat.A_Bx)
  case object GETTABLE  extends OpImpl (OpcodeFormat.A_B_C)
  case object SETGLOBAL  extends OpImpl (OpcodeFormat.A_Bx)
  case object SETUPVAL  extends OpImpl (OpcodeFormat.A_B)
  case object SETTABLE  extends OpImpl (OpcodeFormat.A_B_C)
  case object NEWTABLE  extends OpImpl (OpcodeFormat.A_B_C)
  case object SELF  extends OpImpl (OpcodeFormat.A_B_C)
  case object ADD  extends OpImpl (OpcodeFormat.A_B_C)
  case object SUB  extends OpImpl (OpcodeFormat.A_B_C)
  case object MUL  extends OpImpl (OpcodeFormat.A_B_C)
  case object DIV  extends OpImpl (OpcodeFormat.A_B_C)
  case object MOD  extends OpImpl (OpcodeFormat.A_B_C)
  case object POW  extends OpImpl (OpcodeFormat.A_B_C)
  case object UNM  extends OpImpl (OpcodeFormat.A_B)
  case object NOT  extends OpImpl (OpcodeFormat.A_B)
  case object LEN  extends OpImpl (OpcodeFormat.A_B)
  case object CONCAT  extends OpImpl (OpcodeFormat.A_B_C)
  case object JMP  extends OpImpl (OpcodeFormat.sBx) // Different in 5.2
  case object EQ  extends OpImpl (OpcodeFormat.A_B_C)
  case object LT  extends OpImpl (OpcodeFormat.A_B_C)
  case object LE  extends OpImpl (OpcodeFormat.A_B_C)
  case object TEST  extends OpImpl (OpcodeFormat.A_C)
  case object TESTSET  extends OpImpl (OpcodeFormat.A_B_C)
  case object CALL  extends OpImpl (OpcodeFormat.A_B_C)
  case object TAILCALL  extends OpImpl (OpcodeFormat.A_B_C)
  case object RETURN  extends OpImpl (OpcodeFormat.A_B)
  case object FORLOOP  extends OpImpl (OpcodeFormat.A_sBx)
  case object FORPREP  extends OpImpl (OpcodeFormat.A_sBx)
  case object TFORLOOP  extends OpImpl (OpcodeFormat.A_C)
  case object SETLIST  extends OpImpl (OpcodeFormat.A_B_C)
  case object CLOSE  extends OpImpl (OpcodeFormat.A)
  case object CLOSURE  extends OpImpl (OpcodeFormat.A_Bx)
  case object VARARG  extends OpImpl (OpcodeFormat.A_B)
  // Lua 5.2 Opcodes
  case object LOADKX  extends OpImpl (OpcodeFormat.A)
  case object GETTABUP  extends OpImpl (OpcodeFormat.A_B_C)
  case object SETTABUP  extends OpImpl (OpcodeFormat.A_B_C)
  case object TFORCALL  extends OpImpl (OpcodeFormat.A_C)
  case object EXTRAARG  extends OpImpl (OpcodeFormat.Ax)

}
