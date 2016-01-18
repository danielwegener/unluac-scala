package unluac.decompile.block

import unluac.decompile.Output
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class ElseEndBlock(function: LFunction, begin: Int, end: Int) extends Block(function, begin, end) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer[Statement](end - begin + 1)
  var partner: IfThenElseBlock = null


  override def compareTo(block: Block): Int = {
    if (block eq partner) {
      1
    }
    else {
      val result: Int = super.compareTo(block)
      if (result == 0 && block.isInstanceOf[ElseEndBlock]) {
        System.out.println("HEY HEY HEY")
      }
      result
    }
  }

  def breakable: Boolean = {
    false
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
    if (statements.size == 1 && statements(0).isInstanceOf[IfThenEndBlock]) {
      out.print("else")
      statements(0).print(out)
    }
    else if (statements.size == 2 && statements(0).isInstanceOf[IfThenElseBlock] && statements(1).isInstanceOf[ElseEndBlock]) {
      out.print("else")
      statements(0).print(out)
      statements(1).print(out)
    }
    else {
      out.print("else")
      out.println()
      out.indent()
      Statement.printSequence(out, statements)
      out.dedent()
      out.print("end")
    }
  }
}