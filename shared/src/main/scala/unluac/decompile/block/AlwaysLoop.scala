package unluac.decompile.block

import unluac.decompile.Output
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class AlwaysLoop(function: LFunction, begin: Int, end: Int) extends Block(function,begin,end) {
  private final val statements: mutable.ArrayBuffer[Statement] = new ArrayBuffer[Statement]()


  override def scopeEnd: Int = {
    end - 2
  }

  def breakable: Boolean = {
    true
  }

  def isContainer: Boolean = {
    true
  }

  def isUnprotected: Boolean = {
    true
  }

  def getLoopback: Int = {
    begin
  }

  def print(out: Output) {
    out.println("while true do")
    out.indent
    Statement.printSequence(out, statements)
    out.dedent
    out.print("end")
  }

  def addStatement(statement: Statement) {
    statements += statement
  }
}