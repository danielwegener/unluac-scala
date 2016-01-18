package unluac.decompile.expression

import unluac.decompile.Output

case class Vararg(length: Int, multiple: Boolean) extends Expression(Expression.PRECEDENCE_ATOMIC) {


  def getConstantIndex: Int = {
    -1
  }

  def print(out: Output) {
    out.print(if (multiple) "..." else "(...)")
  }

  override def printMultiple(out: Output) {
    out.print(if (multiple) "..." else "(...)")
  }

  override def isMultiple: Boolean = {
    multiple
  }
}