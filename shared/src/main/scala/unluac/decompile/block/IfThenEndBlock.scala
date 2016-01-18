package unluac.decompile.block

import unluac.decompile.{Declaration, Decompiler, Output, Registers}
import unluac.decompile.branch.{Branch, TestNode}
import unluac.decompile.expression.{BinaryExpression, Expression, LocalVariable}
import unluac.decompile.operation.Operation
import unluac.decompile.statement.{Assignment, Statement}
import unluac.parse.LFunction
import unluac.util.Stack

import scala.collection.mutable.ArrayBuffer

class IfThenEndBlock(function: LFunction, val branch: Branch, val stack: Stack[Branch], val r: Registers) extends Block(function, if (branch.begin == branch.end) branch.begin - 1 else branch.begin, if (branch.begin == branch.end) branch.begin - 1 else branch.end) {
  private final val statements: collection.mutable.ArrayBuffer[Statement] =  ArrayBuffer.empty


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

  def print(out: Output) {
    out.print("if ")
    branch.asExpression(r).print(out)
    out.print(" then")
    out.println()
    out.indent()
    Statement.printSequence(out, statements)
    out.dedent()
    out.print("end")
  }

  override def process(d: Decompiler): Operation = {
    if (statements.size == 1) {
      val stmt: Statement = statements.head
      stmt match {
        case assign: Assignment =>
          if (assign.getArity == 1) {
            branch match {
              case node: TestNode =>
                val decl: Declaration = r.getDeclaration(node.test, node.line)
                if (assign.getFirstTarget.isDeclaration(decl)) {
                  val expr: Expression =
                    if (node.inverted) {
                      new BinaryExpression("or", new LocalVariable(decl), assign.getFirstValue, Expression.PRECEDENCE_OR, Expression.ASSOCIATIVITY_NONE)
                    }
                    else {
                      new BinaryExpression("and", new LocalVariable(decl), assign.getFirstValue, Expression.PRECEDENCE_AND, Expression.ASSOCIATIVITY_NONE)
                    }
                  return new Operation(end - 1) {
                    def process(r: Registers, block: Block): Statement = {
                      new Assignment(assign.getFirstTarget, expr)
                    }
                  }
                }
              case _ =>
            }
          }
        case _ =>
      }
    }
    else if (statements.isEmpty && stack != null) {
      var test: Int = branch.getRegister
      if (test < 0) {
          var reg: Int = 0
          var whileBroken: Boolean = false
          while (reg < r.registers && !whileBroken) {
              if (r.getUpdated(reg, branch.end - 1) >= branch.begin) {
                if (test >= 0) {
                  test = -1
                  whileBroken = true
                }
                if(!whileBroken) test = reg
              }
              if(!whileBroken) reg += 1
          }
      }
      if (test >= 0) {
        if (r.getUpdated(test, branch.end - 1) >= branch.begin) {
          val right: Expression = r.getValue(test, branch.end)
          val setb: Branch = d.popSetCondition(stack, stack.peek.end)
          setb.useExpression(right)
          val testreg: Int = test
          return new Operation(end - 1) {
            def process(r: Registers, block: Block): Statement = {
              r.setValue(testreg, branch.end - 1, setb.asExpression(r))
              null
            }
          }
        }
      }
    }
    super.process(d)
  }
}