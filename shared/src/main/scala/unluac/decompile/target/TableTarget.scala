package unluac.decompile.target

import unluac.decompile.Output
import unluac.decompile.expression.Expression
import unluac.decompile.expression.TableReference

case class TableTarget(table: Expression, index: Expression) extends Target {

  def print(out: Output) {
    new TableReference(table, index).print(out)
  }

  def printMethod(out: Output) {
    table.print(out)
    out.print(":")
    out.print(index.asName)
  }

  override def isFunctionName: Boolean = {
    index.isIdentifier && table.isDotChain
  }
}