package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Assignment
import unluac.decompile.statement.Statement

class RegisterSet(_line: Int, val register: Int, val value: Expression) extends Operation(_line) {


  def process(r: Registers, block: Block): Statement = {
    r.setValue(register, line, value)
    if (r.isAssignable(register, line)) {
      new Assignment(r.getTarget(register, line), value)
    }
    else {
      null
    }
  }
}