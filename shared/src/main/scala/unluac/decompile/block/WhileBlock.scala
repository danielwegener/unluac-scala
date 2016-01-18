package unluac.decompile.block

import unluac.decompile.{Output, Registers}
import unluac.decompile.branch.Branch
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class WhileBlock(function: LFunction, val branch: Branch, val loopback: Int, val r: Registers) extends Block(function, branch.begin, branch.end) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer[Statement](branch.end - branch.begin + 1)


  override def scopeEnd: Int = {
    end - 2
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
    true
  }

  def getLoopback: Int = {
    loopback
  }

  def print(out: Output) {
    out.print("while ")
    branch.asExpression(r).print(out)
    out.print(" do")
    out.println()
    out.indent()
    Statement.printSequence(out, statements)
    out.dedent()
    out.print("end")
  }
}