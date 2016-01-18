package unluac.decompile.statement


import unluac.decompile.Output
import unluac.decompile.expression.Expression

class Return(var values: Array[Expression]) extends Statement {

  def this() {
    this(Array.empty[Expression])
  }

  def this(value: Expression) {
    this(Array(value))
  }


  def print(out: Output) {
    out.print("do ")
    printTail(out)
    out.print(" end")
  }

  override def printTail(out: Output) {
    out.print("return")
    if (values.nonEmpty) {
      out.print(" ")
      Expression.printSequence(out, values.toSeq, false, true)
    }
  }
}