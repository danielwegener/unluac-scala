package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.Expression
import unluac.decompile.expression.UnaryExpression

case class NotBranch(val branch: Branch) extends Branch(branch.line, branch.begin, branch.end) {

  def invert: Branch = {
    branch
  }

  def getRegister: Int = {
    branch.getRegister
  }

  def asExpression(r: Registers): Expression = {
    new UnaryExpression("not ", branch.asExpression(r), Expression.PRECEDENCE_UNARY)
  }

  def useExpression(expression: Expression) {
  }
}