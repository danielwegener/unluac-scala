package unluac.decompile.expression

import unluac.decompile.Output

class UnaryExpression(val op: String, val expression: Expression, precedence: Int) extends Expression(precedence) {

  def getConstantIndex: Int = {
    expression.getConstantIndex
  }

  def print(out: Output) {
    out.print(op)
    if (precedence > expression.precedence) out.print("(")
    expression.print(out)
    if (precedence > expression.precedence) out.print(")")
  }
}