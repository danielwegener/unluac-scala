package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.expression.FunctionCall
import unluac.decompile.statement.FunctionCallStatement
import unluac.decompile.statement.Statement

class CallOperation(line: Int, var call: FunctionCall) extends Operation(line) {

  def process(r: Registers, block: Block): Statement = {
    new FunctionCallStatement(call)
  }
}