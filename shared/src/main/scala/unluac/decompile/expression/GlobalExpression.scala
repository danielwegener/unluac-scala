package unluac.decompile.expression

import unluac.decompile.Output

case class GlobalExpression(name: String, index: Int) extends Expression(Expression.PRECEDENCE_ATOMIC) {

  def getConstantIndex: Int = {
    index
  }

  override def isDotChain: Boolean = {
    true
  }

  def print(out: Output) {
    out.print(name)
  }

  override def isBrief: Boolean = {
    true
  }
}