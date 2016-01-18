package unluac.decompile.expression

import unluac.decompile.Output

case class UpvalueExpression(name:String) extends Expression(Expression.PRECEDENCE_ATOMIC) {

  def getConstantIndex: Int = {
    -1
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