package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.Expression

class TestSetNode(target: Int, val test: Int, val inverted: Boolean, line: Int, begin: Int, end: Int) extends Branch(line, begin, end) {
  setTarget = target


  def invert: Branch = {
    new TestSetNode(setTarget, test, !inverted, line, end, begin)
  }

  def getRegister: Int = {
    setTarget
  }

  def asExpression(r: Registers): Expression = {
    r.getExpression(test, line)
  }

  def useExpression(expression: Expression) {
  }

  override def toString: String = {
    "TestSetNode[target=" + setTarget + ";test=" + test + ";inverted=" + inverted + ";line=" + line + ";begin=" + begin + ";end=" + end + "]"
  }
}