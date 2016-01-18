package unluac.decompile.operation

import unluac.decompile.Registers
import unluac.decompile.block.Block
import unluac.decompile.expression.Expression
import unluac.decompile.expression.TableLiteral
import unluac.decompile.statement.Assignment
import unluac.decompile.statement.Statement
import unluac.decompile.target.TableTarget

class TableSet(line: Int, var table: Expression, var index: Expression, var value: Expression, var isTable: Boolean, var timestamp: Int) extends Operation(line) {

  def process(r: Registers, block: Block): Statement = {
    if (table.isTableLiteral) {
      table.addEntry(new TableLiteral.Entry(index, value, !isTable, timestamp))
      null
    }
    else {
      new Assignment(new TableTarget(table, index), value)
    }
  }
}