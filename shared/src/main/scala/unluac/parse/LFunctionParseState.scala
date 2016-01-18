package unluac.parse

class LFunctionParseState {
  var name: LString = null
  private[parse] var lineBegin: Int = 0
  private[parse] var lineEnd: Int = 0
  private[parse] var lenUpvalues: Int = 0
  private[parse] var lenParameter: Int = 0
  private[parse] var vararg: Int = 0
  private[parse] var maximumStackSize: Int = 0
  private[parse] var length: Int = 0
  private[parse] var code: Array[Int] = null
  private[parse] var constants: BList[LObject] = null
  private[parse] var functions: BList[LFunction] = null
  private[parse] var lines: BList[BInteger] = null
  private[parse] var locals: BList[LLocal] = null
  private[parse] var upvalues: Array[LUpvalue] = null
}