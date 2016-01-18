package unluac.decompile.block

import java.util

import unluac.decompile.{Output, Registers}
import unluac.decompile.expression.Expression
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class TForBlock(function: LFunction, begin: Int, end: Int, val register: Int, val length: Int, val r: Registers) extends Block(function, begin, end) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer(end - begin + 1)


  override def scopeEnd: Int = {
    end - 3
  }

  def breakable: Boolean = {
    true
  }

  def isContainer: Boolean = {
    true
  }

  def addStatement(statement: Statement) {
    statements += statement
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

    var r1: Int = register + 4
    while (r1 <= register + 2 + length) {
      {
        out.print(", ")
        r.getTarget(r1, begin - 1).print(out)
      }
      {
        r1 += 1
        r1 - 1
      }
    }

    out.print(" in ")
    var value: Expression = null
    value = r.getValue(register, begin - 1)
    value.print(out)
    if (!value.isMultiple) {
      out.print(", ")
      value = r.getValue(register + 1, begin - 1)
      value.print(out)
      if (!value.isMultiple) {
        out.print(", ")
        value = r.getValue(register + 2, begin - 1)
        value.print(out)
      }
    }
    out.print(" do")
    out.println()
    out.indent()
    Statement.printSequence(out, statements)
    out.dedent()
    out.print("end")
  }
}