package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Assignment

class AssignNode(line: Int, begin: Int, end: Int) extends Branch(line, begin, end) {
  private var expression: Expression = null


  def invert: Branch = {
    throw new IllegalStateException
  }

  def getRegister: Int = {
    throw new IllegalStateException
  }

  def asExpression(r: Registers): Expression = {
    expression
  }

  def useExpression(expression: Expression) {
    this.expression = expression
  }
}