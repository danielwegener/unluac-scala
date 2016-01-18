package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.statement.Statement

abstract class Operation(val line:Int) {

  def process(r: Registers, block: Block): Statement
}