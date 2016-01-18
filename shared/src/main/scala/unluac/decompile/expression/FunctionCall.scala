package unluac.decompile.expression

import unluac.decompile.Output
import scala.collection.mutable.ArrayBuffer


case class FunctionCall(function: Expression, arguments: Array[Expression], multiple: Boolean) extends Expression(Expression.PRECEDENCE_ATOMIC) {


  def getConstantIndex: Int = {
    var index: Int = function.getConstantIndex
    for (argument <- arguments) {
      index = Math.max(argument.getConstantIndex, index)
    }
    index
  }

  override def isMultiple: Boolean = {
    multiple
  }

  override def printMultiple(out: Output) {
    if (!multiple) {
      out.print("(")
    }
    print(out)
    if (!multiple) {
      out.print(")")
    }
  }

  private def isMethodCall: Boolean = {
    function.isMemberAccess && arguments.length > 0 && (function.getTable eq arguments(0))
  }

  override def beginsWithParen: Boolean = {
    if (isMethodCall) {
      val obj: Expression = function.getTable
      obj.isClosure || obj.isConstant || obj.beginsWithParen
    }
    else {
      function.isClosure || function.isConstant || function.beginsWithParen
    }
  }

  def print(out: Output) {
    val args: ArrayBuffer[Expression] = new ArrayBuffer[Expression](arguments.length)
    if (isMethodCall) {
      val obj: Expression = function.getTable
      if (obj.isClosure || obj.isConstant) {
        out.print("(")
        obj.print(out)
        out.print(")")
      }
      else {
        obj.print(out)
      }
      out.print(":")
      out.print(function.getField)
        var i: Int = 1
        while (i < arguments.length) {
            args += arguments(i)
            i += 1
        }
    }
    else {
      if (function.isClosure || function.isConstant) {
        out.print("(")
        function.print(out)
        out.print(")")
      }
      else {
        function.print(out)
      }
        var i: Int = 0
        while (i < arguments.length) {
            args += arguments(i)
            i += 1
        }
    }
    out.print("(")
    Expression.printSequence(out, args, false, true)
    out.print(")")
  }
}