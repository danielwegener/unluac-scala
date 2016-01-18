package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.BinaryExpression
import unluac.decompile.expression.Expression
import unluac.decompile.expression.UnaryExpression

class LTNode(val left: Int, val right: Int, val inverted: Boolean, line: Int, begin: Int, end: Int) extends Branch(line, begin, end) {

  def invert: Branch = {
    new LTNode(left, right, !inverted, line, end, begin)
  }

  def getRegister: Int = {
    -1
  }

  def asExpression(r: Registers): Expression = {
    var transpose: Boolean = false
    val leftExpression: Expression = r.getKExpression(left, line)
    val rightExpression: Expression = r.getKExpression(right, line)
    if (((left | right) & 256) == 0) {
      transpose = r.getUpdated(left, line) > r.getUpdated(right, line)
    }
    else {
      transpose = rightExpression.getConstantIndex < leftExpression.getConstantIndex
    }
    val op: String = if (!transpose) "<" else ">"
    var rtn: Expression = new BinaryExpression(op, if (!transpose) leftExpression else rightExpression, if (!transpose) rightExpression else leftExpression, Expression.PRECEDENCE_COMPARE, Expression.ASSOCIATIVITY_LEFT)
    if (inverted) {
      rtn = new UnaryExpression("not ", rtn, Expression.PRECEDENCE_UNARY)
    }
    rtn
  }

  def useExpression(expression: Expression) {
  }
}