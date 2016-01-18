package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.BinaryExpression
import unluac.decompile.expression.Expression

case class OrBranch(left: Branch, right: Branch) extends Branch(right.line, right.begin, right.end) {

  def invert: Branch = {
    new AndBranch(left.invert, right.invert)
  }

  def getRegister: Int = {
    val rleft: Int = left.getRegister
    val rright: Int = right.getRegister
    if (rleft == rright) rleft else -1
  }

  def asExpression(r: Registers): Expression = {
    new BinaryExpression("or", left.asExpression(r), right.asExpression(r), Expression.PRECEDENCE_OR, Expression.ASSOCIATIVITY_NONE)
  }

  def useExpression(expression: Expression) {
    left.useExpression(expression)
    right.useExpression(expression)
  }
}