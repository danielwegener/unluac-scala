package unluac.decompile.block

import unluac.decompile.Output
import unluac.decompile.statement.{Return, Statement}
import unluac.parse.LFunction

import scala.collection.mutable.ArrayBuffer

class OuterBlock(function: LFunction, length: Int) extends Block(function, 0, length + 1) {
  private final val statements: ArrayBuffer[Statement] = new ArrayBuffer(length)


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

  override def scopeEnd: Int = {
    (end - 1) + function.header.version.getOuterBlockScopeAdjustment
  }

  def print(out: Output) {
    val last: Int = statements.size - 1
    if (last < 0 || !statements.last.isInstanceOf[Return]) {
      throw new IllegalStateException(statements.last.toString)
    }
    statements.remove(last)
    Statement.printSequence(out, statements)
  }
}