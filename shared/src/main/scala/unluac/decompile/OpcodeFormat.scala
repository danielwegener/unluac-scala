package unluac.decompile

sealed trait OpcodeFormat
object OpcodeFormat {
  case object A extends OpcodeFormat
  case object A_B extends OpcodeFormat
  case object A_C extends OpcodeFormat
  case object A_B_C extends OpcodeFormat
  case object A_Bx extends OpcodeFormat
  case object A_sBx extends OpcodeFormat
  case object Ax extends OpcodeFormat
  case object sBx extends OpcodeFormat
}
