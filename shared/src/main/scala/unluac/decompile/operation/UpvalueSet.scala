package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Assignment
import unluac.decompile.statement.Statement
import unluac.decompile.target.UpvalueTarget

class UpvalueSet(line: Int, upvalue: String, val value: Expression) extends Operation(line) {
  private var target: UpvalueTarget = new UpvalueTarget(upvalue)

  def process(r: Registers, block: Block): Statement = {
    new Assignment(target, value)
  }
}