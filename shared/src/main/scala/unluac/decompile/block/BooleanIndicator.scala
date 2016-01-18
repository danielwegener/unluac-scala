package unluac.decompile.block

import unluac.decompile.Output
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

class BooleanIndicator(function: LFunction, line: Int) extends Block(function, line, line) {

  def addStatement(statement: Statement) {
  }

  def isContainer: Boolean = {
    false
  }

  def isUnprotected: Boolean = {
    false
  }

  def breakable: Boolean = {
    false
  }

  def getLoopback: Int = {
    throw new IllegalStateException
  }

  def print(out: Output) {
    out.print("-- unhandled boolean indicator")
  }
}