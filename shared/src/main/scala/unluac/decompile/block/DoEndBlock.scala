package unluac.decompile.block

import unluac.decompile.Output
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class DoEndBlock(function: LFunction, begin: Int, end: Int) extends Block(function, begin, end) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer(end - begin + 1)

  def addStatement(statement: Statement) {
    statements += statement
  }

  def breakable: Boolean = {
    false
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
    out.println("do")
    out.indent()
    Statement.printSequence(out, statements)
    out.dedent()
    out.print("end")
  }
}