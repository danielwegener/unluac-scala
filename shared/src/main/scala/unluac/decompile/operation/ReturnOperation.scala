package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Return
import unluac.decompile.statement.Statement

class ReturnOperation(line: Int, val values: Array[Expression]) extends Operation(line) {

  def this(line: Int, value: Expression) = {
   this(line, Array(value))
  }


  def process(r: Registers, block: Block): Statement = {
    new Return(values)
  }
}