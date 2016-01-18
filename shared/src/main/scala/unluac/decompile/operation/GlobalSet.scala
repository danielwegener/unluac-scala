package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Assignment
import unluac.decompile.statement.Statement
import unluac.decompile.target.GlobalTarget

class GlobalSet(line: Int, var global: String, var value: Expression) extends Operation(line) {

  def process(r: Registers, block: Block): Statement = {
    new Assignment(new GlobalTarget(global), value)
  }
}