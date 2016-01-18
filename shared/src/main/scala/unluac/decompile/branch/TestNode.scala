package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.Expression

class TestNode(val test: Int, val inverted: Boolean, line: Int, begin: Int, end: Int) extends Branch(line, begin, end) {


  def invert: Branch = {
    new TestNode(test, !inverted, line, end, begin)
  }

  def getRegister: Int = {
    test
  }

  def asExpression(r: Registers): Expression = {
    if (inverted) {
      new NotBranch(this.invert).asExpression(r)
    }
    else {
      r.getExpression(test, line)
    }
  }

  def useExpression(expression: Expression) {
  }

  override def toString: String = {
    "TestNode[test=" + test + ";inverted=" + inverted + ";line=" + line + ";begin=" + begin + ";end=" + end + "]"
  }
}