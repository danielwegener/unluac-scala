package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.BinaryExpression
import unluac.decompile.expression.Expression

class EQNode(val left: Int, val right: Int, private val inverted: Boolean, line: Int, begin: Int, end: Int) extends Branch(line, begin, end) {


  def invert: Branch = {
    new EQNode(left, right, !inverted, line, end, begin)
  }

  def getRegister: Int = {
    -1
  }

  def asExpression(r: Registers): Expression = {
    val transpose: Boolean = false
    val op: String = if (inverted) "~=" else "=="
    new BinaryExpression(op, r.getKExpression(if (!transpose) left else right, line), r.getKExpression(if (!transpose) right else left, line), Expression.PRECEDENCE_COMPARE, Expression.ASSOCIATIVITY_LEFT)
  }

  def useExpression(expression: Expression) {
  }
}