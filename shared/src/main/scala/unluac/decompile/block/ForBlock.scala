package unluac.decompile.block

import unluac.decompile.{Output, Registers}
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class ForBlock(function: LFunction, begin: Int, end: Int, val register: Int, val r: Registers) extends Block(function, begin, end) {
  private final val statements: ArrayBuffer[Statement] =  new ArrayBuffer(end - begin + 1)


  override def scopeEnd: Int = {
    end - 2
  }

  def addStatement(statement: Statement) {
    statements += statement
  }

  def breakable: Boolean = {
    true
  }

  def isContainer: Boolean = {
    true
  }

  def isUnprotected: Boolean = {
    false
  }

  def getLoopback: Int = {
    throw new IllegalStateException
  }

  def print(out: Output) {
    out.print("for ")
    r.getTarget(register + 3, begin - 1).print(out)
    out.print(" = ")
    r.getValue(register, begin - 1).print(out)
    out.print(", ")
    r.getValue(register + 1, begin - 1).print(out)
    val step: Expression = r.getValue(register + 2, begin - 1)
    if (!step.isInteger || step.asInteger != 1) {
      out.print(", ")
      step.print(out)
    }
    out.print(" do")
    out.println()
    out.indent()
    Statement.printSequence(out, statements)
    out.dedent()
    out.print("end")
  }
}