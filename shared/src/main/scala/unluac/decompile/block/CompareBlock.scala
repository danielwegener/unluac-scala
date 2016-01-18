package unluac.decompile.block

import unluac.decompile.{Decompiler, Output, Registers}
import unluac.decompile.branch.Branch
import unluac.decompile.operation.{Operation, RegisterSet}
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

class CompareBlock(function: LFunction, begin: Int, end: Int, val target: Int, val branch: Branch) extends Block(function, begin, end) {

  def isContainer: Boolean = {
    false
  }

  def breakable: Boolean = {
    false
  }

  def addStatement(statement: Statement) {
  }

  def isUnprotected: Boolean = {
    false
  }

  def getLoopback: Int = {
    throw new IllegalStateException
  }

  def print(out: Output) {
    out.print("-- unhandled compare assign")
  }

  override def process(d: Decompiler): Operation = {
    new Operation(end - 1) {
      def process(r: Registers, block: Block): Statement = {
        new RegisterSet(end - 1, target, branch.asExpression(r)).process(r, block)
      }
    }
  }
}