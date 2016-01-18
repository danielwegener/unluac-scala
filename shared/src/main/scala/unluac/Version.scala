package unluac

import unluac.decompile.{Op, OpcodeMap}
import unluac.parse.LFunctionType

sealed trait Version {

  val versionNumber:Int
  val hasHeaderTail: Boolean
  val getLFunctionType: LFunctionType
  val getOuterBlockScopeAdjustment: Int
  val usesOldLoadNilEncoding: Boolean
  val usesInlineUpvalueDeclarations: Boolean
  val getTForTarget: Op
  def isBreakableLoopEnd(op: Op): Boolean
  final def getOpcodeMap: OpcodeMap = { new OpcodeMap(versionNumber) }
}

object Version {
  case object  LUA51 extends Version {
    override val versionNumber = 0x51
    override val hasHeaderTail: Boolean = false
    override val getLFunctionType: LFunctionType = LFunctionType.TYPE51
    override val getOuterBlockScopeAdjustment: Int = -1
    override val usesOldLoadNilEncoding: Boolean = true
    override val usesInlineUpvalueDeclarations: Boolean = true
    override val getTForTarget: Op = Op.TFORLOOP
    override def isBreakableLoopEnd(op: Op): Boolean = {
      op == Op.JMP || op == Op.FORLOOP
    }
  }

  case object LUA52 extends Version {
    override val versionNumber = 0x52
    override val hasHeaderTail: Boolean = true
    override val getLFunctionType: LFunctionType = LFunctionType.TYPE52
    override val getOuterBlockScopeAdjustment: Int = 0
    override val usesOldLoadNilEncoding: Boolean = false
    override val usesInlineUpvalueDeclarations: Boolean = false
    override val getTForTarget: Op = Op.TFORCALL
    def isBreakableLoopEnd(op: Op): Boolean = {
      op == Op.JMP || op == Op.FORLOOP || op == Op.TFORLOOP
    }
  }
}




