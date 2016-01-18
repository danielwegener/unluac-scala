package unluac.decompile.block

import unluac.decompile.{Output, Registers}
import unluac.decompile.branch.Branch
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class RepeatBlock(function: LFunction, val branch: Branch, val r: Registers) extends Block(function, branch.end, branch.begin) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer(branch.begin - branch.end + 1)


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
    out.print("repeat")
    out.println()
    out.indent()
    Statement.printSequence(out, statements)
    out.dedent()
    out.print("until ")
    branch.asExpression(r).print(out)
  }
}