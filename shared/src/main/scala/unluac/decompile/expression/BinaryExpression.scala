package unluac.decompile.expression

import unluac.decompile.Output

class BinaryExpression(val op: String, val left: Expression, val right: Expression, precedence: Int, val associativity: Int) extends Expression(precedence) {


  def getConstantIndex: Int = {
    Math.max(left.getConstantIndex, right.getConstantIndex)
  }

  override def beginsWithParen: Boolean = {
    leftGroup || left.beginsWithParen
  }

  def print(out: Output) {
    if (leftGroup) out.print("(")
    left.print(out)
    if (leftGroup) out.print(")")
    out.print(" ")
    out.print(op)
    out.print(" ")
    if (rightGroup) out.print("(")
    right.print(out)
    if (rightGroup) out.print(")")
  }

  private def leftGroup: Boolean = {
    precedence > left.precedence || (precedence == left.precedence && associativity == Expression.ASSOCIATIVITY_RIGHT)
  }

  private def rightGroup: Boolean = {
    precedence > right.precedence || (precedence == right.precedence && associativity == Expression.ASSOCIATIVITY_LEFT)
  }
}