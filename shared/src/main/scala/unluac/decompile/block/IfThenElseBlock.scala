package unluac.decompile.block

import unluac.decompile.{Output, Registers}
import unluac.decompile.branch.Branch
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class IfThenElseBlock(function: LFunction, val branch: Branch, val loopback: Int, val emptyElse: Boolean, val r: Registers) extends Block(function, branch.begin, branch.end) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer(branch.end - branch.begin + 1)
  var partner: ElseEndBlock = null


  override def compareTo(block: Block): Int = {
    if (block == partner) {
      -1
    }
    else {
      super.compareTo(block)
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

  override def scopeEnd: Int = {
    end - 2
  }

  def isUnprotected: Boolean = {
    true
  }

  def getLoopback: Int = {
    loopback
  }

  def print(out: Output) {
    out.print("if ")
    branch.asExpression(r).print(out)
    out.print(" then")
    out.println()
    out.indent()
    if (statements.size == 1 && statements.head.isInstanceOf[Break]) {
      val b: Break = statements.head.asInstanceOf[Break]
      if (b.target == loopback) {
        out.dedent()
        return
      }
    }
    Statement.printSequence(out, statements)
    out.dedent()
    if (emptyElse) {
      out.println("else")
      out.println("end")
    }
  }
}