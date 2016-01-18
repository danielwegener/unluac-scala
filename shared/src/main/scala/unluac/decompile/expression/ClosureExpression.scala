package unluac.decompile.expression

import unluac.decompile.Declaration
import unluac.decompile.Decompiler
import unluac.decompile.Output
import unluac.decompile.target.TableTarget
import unluac.decompile.target.Target
import unluac.decompile.target.VariableTarget
import unluac.parse.LFunction
import unluac.parse.LUpvalue

case class ClosureExpression(function: LFunction, upvalueLine: Int, declList: Array[Declaration]) extends Expression(Expression.PRECEDENCE_ATOMIC) {

  def this(function: LFunction, declList: Array[Declaration], upvalueLine: Int) {
    this(function, upvalueLine, declList)
  }

  def getConstantIndex: Int = {
    -1
  }

  override def isClosure: Boolean = {
    true
  }

  override def isUpvalueOf(register: Int): Boolean = {

      var i: Int = 0
      while (i < function.upvalues.length) {
        {
          val upvalue: LUpvalue = function.upvalues(i)
          if (upvalue.instack && upvalue.idx == register) {
            return true
          }
          i += 1
        }
      }

    false
  }

  override def closureUpvalueLine: Int = {
    upvalueLine
  }

  def print(out: Output) {
    val d: Decompiler = new Decompiler(function)
    out.print("function")
    printMain(out, d, includeFirst = true)
  }

  override def printClosure(out: Output, name: Target) {
    val d: Decompiler = new Decompiler(function)
    out.print("function ")
    if (function.numParams >= 1 && (d.declList(0).name == "self") && name.isInstanceOf[TableTarget]) {
      name.printMethod(out)
      printMain(out, d, includeFirst = false)
    }
    else {
      name.print(out)
      printMain(out, d, includeFirst = true)
    }
  }

  private def printMain(out: Output, d: Decompiler, includeFirst: Boolean) {
    out.print("(")
    val start: Int = if (includeFirst) 0 else 1
    if (function.numParams > start) {
      new VariableTarget(d.declList(start)).print(out)
        var i: Int = start + 1
        while (i < function.numParams) {
            out.print(", ")
            new VariableTarget(d.declList(i)).print(out)
            i += 1
        }
    }
    if ((function.vararg & 1) == 1) {
      if (function.numParams > start) {
        out.print(", ...")
      }
      else {
        out.print("...")
      }
    }
    out.print(")")
    out.println()
    out.indent()
    val b = d.decompile()
    d.print(out, b)
    out.dedent()
    out.print("end")
  }
}