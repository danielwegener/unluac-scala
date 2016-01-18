package unluac.decompile.statement

import unluac.decompile.Output
import unluac.decompile.block.IfThenElseBlock

object Statement {
  /**
    * Prints out a sequences of statements on separate lines. Correctly
    * informs the last statement that it is last in a block.
    */
  def printSequence(out: Output, stmts: IndexedSeq[Statement]) {
    val n: Int = stmts.size
      var i: Int = 0
      while (i < n) {
        {
          val last: Boolean = i + 1 == n
          val stmt: Statement = stmts(i)
          val next: Statement = if (last) null else stmts(i + 1)
          if (last) {
            stmt.printTail(out)
          }
          else {
            stmt.print(out)
          }
          if (next != null && stmt.isInstanceOf[FunctionCallStatement] && next.beginsWithParen) {
            out.print(";")
          }
          if (!stmt.isInstanceOf[IfThenElseBlock]) {
            out.println()
          }
          i += 1
        }
      }
  }
}

abstract class Statement {
  def print(out: Output)

  def printTail(out: Output) {
    print(out)
  }

  var comment: String = null

  def addComment(comment: String) {
    this.comment = comment
  }

  def beginsWithParen: Boolean = {
    false
  }
}