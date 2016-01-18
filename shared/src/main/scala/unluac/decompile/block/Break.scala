package unluac.decompile.block

import unluac.decompile.Output
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

class Break(function: LFunction, line: Int, val target: Int) extends Block(function, line, line) {

  def addStatement(statement: Statement) {
    throw new IllegalStateException
  }

  def isContainer: Boolean = {
    false
  }

  def breakable: Boolean = {
    false
  }

  def isUnprotected: Boolean = {
    false
  }

  def getLoopback: Int = {
    throw new IllegalStateException
  }

  def print(out: Output) {
    out.print("do break end")
  }

  override def printTail(out: Output) {
    out.print("break")
  }
}