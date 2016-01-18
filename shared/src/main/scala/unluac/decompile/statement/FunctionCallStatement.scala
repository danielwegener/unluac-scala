package unluac.decompile.statement

import unluac.decompile.Output
import unluac.decompile.expression.FunctionCall

class FunctionCallStatement(val call: FunctionCall) extends Statement {

  def print(out: Output) {
    call.print(out)
  }

  override def beginsWithParen: Boolean = {
    call.beginsWithParen
  }
}