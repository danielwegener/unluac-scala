package unluac.decompile.block

import unluac.decompile.{Decompiler, Op, Output, Registers}
import unluac.decompile.branch.Branch
import unluac.decompile.expression.Expression
import unluac.decompile.operation.{Operation, RegisterSet}
import unluac.decompile.statement.{Assignment, Statement}
import unluac.decompile.target.Target
import unluac.parse.LFunction


class SetBlock(function: LFunction, val branch: Branch, val target: Int, _begin: Int, _end: Int ) extends Block(function, _begin, _end) {
  private var assign: Assignment = null
  private var r: Registers = null
  private var empty: Boolean = false
  private var finalizeV: Boolean = false

  def this(function: LFunction, branch: Branch, target: Int, line: Int, begin: Int, end: Int, empty: Boolean, r: Registers) {
    this(function, branch, target, begin, end)
    this.empty = empty
    if (begin == end) this.begin = begin - 1
    this.r = r
  }

  def addStatement(statement: Statement) {
    if (!finalizeV && statement.isInstanceOf[Assignment]) {
      this.assign = statement.asInstanceOf[Assignment]
    }
    else if (statement.isInstanceOf[BooleanIndicator]) {
      finalizeV = true
    }
  }

  def isUnprotected: Boolean = {
    false
  }

  def getLoopback: Int = {
    throw new IllegalStateException
  }

  def print(out: Output) {
    if (assign != null && assign.getFirstTarget != null) {
      val assignOut: Assignment = new Assignment(assign.getFirstTarget, getValue)
      assignOut.print(out)
    }
    else {
      out.print("-- unhandled set block")
      out.println()
    }
  }

  def breakable: Boolean = {
    false
  }

  def isContainer: Boolean = {
    false
  }

  def useAssignment(assign: Assignment) {
    this.assign = assign
    branch.useExpression(assign.getFirstValue)
  }

  def getValue: Expression = {
    branch.asExpression(r)
  }

  override def process(d: Decompiler): Operation = {
    if (empty) {
      val expression: Expression = r.getExpression(branch.setTarget, end)
      branch.useExpression(expression)
      new RegisterSet(end - 1, branch.setTarget, branch.asExpression(r))
    }
    else if (assign != null) {
      branch.useExpression(assign.getFirstValue)
      val target: Target = assign.getFirstTarget
      val value: Expression = getValue
      new Operation(end - 1) {
        def process(r: Registers, block: Block): Statement = {
          new Assignment(target, value)
        }
      }
    }
    else {
      new Operation(end - 1) {
        def process(r: Registers, block: Block): Statement = {
          var expr: Expression = null
          var register: Int = 0

          var whileBroken = false
          while (register < r.registers && !whileBroken) {
              if (r.getUpdated(register, branch.end - 1) == branch.end - 1) {
                expr = r.getValue(register, branch.end)
                whileBroken = true
              }
              if(!whileBroken)
              register += 1
          }
          if (d.code.op(branch.end - 2) == Op.LOADBOOL && d.code.C(branch.end - 2) != 0) {
            val target: Int = d.code.A(branch.end - 2)
            if (d.code.op(branch.end - 3) == Op.JMP && d.code.sBx(branch.end - 3) == 2) {
              expr = r.getValue(target, branch.end - 2)
            }
            else {
              expr = r.getValue(target, branch.begin)
            }
            branch.useExpression(expr)
            if (r.isLocal(target, branch.end - 1)) {
              return new Assignment(r.getTarget(target, branch.end - 1), branch.asExpression(r))
            }
            r.setValue(target, branch.end - 1, branch.asExpression(r))
          }
          else if (expr != null && target >= 0) {
            branch.useExpression(expr)
            if (r.isLocal(target, branch.end - 1)) {
              return new Assignment(r.getTarget(target, branch.end - 1), branch.asExpression(r))
            }
            r.setValue(target, branch.end - 1, branch.asExpression(r))
          }
          else {
            System.out.println("-- fail " + (branch.end - 1))
            System.out.println(expr)
            System.out.println(target)
          }
          null
        }
      }
    }
  }
}