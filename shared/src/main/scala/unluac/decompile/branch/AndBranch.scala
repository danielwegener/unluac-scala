package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.BinaryExpression
import unluac.decompile.expression.Expression

class AndBranch(val left: Branch, val right: Branch) extends Branch(right.line, right.begin, right.end) {

  def invert: Branch = {
    new OrBranch(left.invert, right.invert)
  }

  def getRegister: Int = {
    val rleft: Int = left.getRegister
    val rright: Int = right.getRegister
    if (rleft == rright) rleft else -1
  }

  def asExpression(r: Registers): Expression = {
    new BinaryExpression("and", left.asExpression(r), right.asExpression(r), Expression.PRECEDENCE_AND, Expression.ASSOCIATIVITY_NONE)
  }

  def useExpression(expression: Expression) {
    left.useExpression(expression)
    right.useExpression(expression)
  }
}