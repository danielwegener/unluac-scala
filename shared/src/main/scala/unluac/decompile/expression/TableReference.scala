package unluac.decompile.expression

import unluac.decompile.Output

case class TableReference(table: Expression, index: Expression) extends Expression(Expression.PRECEDENCE_ATOMIC) {

  def getConstantIndex: Int = {
    Math.max(table.getConstantIndex, index.getConstantIndex)
  }

  def print(out: Output) {
    table.print(out)
    if (index.isIdentifier) {
      out.print(".")
      out.print(index.asName)
    }
    else {
      out.print("[")
      index.print(out)
      out.print("]")
    }
  }

  override def isDotChain: Boolean = {
    index.isIdentifier && table.isDotChain
  }

  override def isMemberAccess: Boolean = {
    index.isIdentifier
  }

  override def getTable: Expression = {
    table
  }

  override def getField: String = {
    index.asName
  }
}