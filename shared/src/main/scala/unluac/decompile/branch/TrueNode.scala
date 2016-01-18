package unluac.decompile.branch

import unluac.decompile.Constant
import unluac.decompile.Registers
import unluac.decompile.expression.ConstantExpression
import unluac.decompile.expression.Expression
import unluac.parse.LBoolean

class TrueNode(val register: Int, val inverted: Boolean, line: Int, begin: Int, end: Int) extends Branch(line, begin, end) {

  setTarget = register

  override def invert: Branch = {
    new TrueNode(register, !inverted, line, end, begin)
  }

  override def getRegister: Int = {
    register
  }

  override def asExpression(r: Registers): Expression = {
    new ConstantExpression(Constant.apply(if (inverted) LBoolean.LTRUE else LBoolean.LFALSE), -1)
  }

  override def useExpression(expression: Expression) {
  }

  override def toString: String = {
    "TrueNode[inverted=" + inverted + ";line=" + line + ";begin=" + begin + ";end=" + end + "]"
  }
}