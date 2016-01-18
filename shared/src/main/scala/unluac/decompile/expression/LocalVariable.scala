package unluac.decompile.expression

import unluac.decompile.Declaration
import unluac.decompile.Output

case class LocalVariable(decl: Declaration) extends Expression(Expression.PRECEDENCE_ATOMIC) {

  def getConstantIndex: Int = {
    -1
  }

  override def isDotChain: Boolean = {
    true
  }

  def print(out: Output) {
    out.print(decl.name)
  }

  override def isBrief: Boolean = {
    true
  }
}