package unluac.decompile.block

import unluac.decompile.{Decompiler, Registers}
import unluac.decompile.operation.Operation
import unluac.decompile.statement.Statement
import unluac.parse.LFunction

abstract class Block(val function: LFunction, var begin: Int, var end: Int) extends Statement with Ordered[Block] {
  var loopRedirectAdjustment: Boolean = false


  def addStatement(statement: Statement)

  def contains(block: Block): Boolean = {
    begin <= block.begin && end >= block.end
  }

  def contains(line: Int): Boolean = {
    begin <= line && line < end
  }

  def scopeEnd: Int = {
    end - 1
  }

  /**
    * An unprotected block is one that ends in a JMP instruction.
    * If this is the case, any inner statement that tries to jump
    * to the end of this block will be redirected.
    *
    * (One of the lua compiler's few optimizations is that is changes
    * any JMP that targets another JMP to the ultimate target. This
    * is what I call redirection.)
    */
  def isUnprotected: Boolean

  def getLoopback: Int

  def breakable: Boolean

  def isContainer: Boolean

  override def compare(block: Block): Int = {
    if (this.begin < block.begin) {
      -1
    }
    else if (this.begin == block.begin) {
      if (this.end < block.end) {
        1
      }
      else if (this.end == block.end) {
        if (this.isContainer && !block.isContainer) {
          -1
        }
        else if (!this.isContainer && block.isContainer) {
          1
        }
        else {
          0
        }
      }
      else {
        -1
      }
    }
    else {
      1
    }
  }

  def process(d: Decompiler): Operation = {
    val statement: Statement = this
    new Operation(end - 1) {
      def process(r: Registers, block: Block): Statement = {
        statement
      }
    }
  }
}