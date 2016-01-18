package unluac.decompile.expression

import java.util
import unluac.decompile.Constant
import unluac.decompile.Output
import unluac.decompile.target.Target
import unluac.parse.LNil

object Expression {
  val PRECEDENCE_OR: Int = 1
  val PRECEDENCE_AND: Int = 2
  val PRECEDENCE_COMPARE: Int = 3
  val PRECEDENCE_CONCAT: Int = 4
  val PRECEDENCE_ADD: Int = 5
  val PRECEDENCE_MUL: Int = 6
  val PRECEDENCE_UNARY: Int = 7
  val PRECEDENCE_POW: Int = 8
  val PRECEDENCE_ATOMIC: Int = 9
  val ASSOCIATIVITY_NONE: Int = 0
  val ASSOCIATIVITY_LEFT: Int = 1
  val ASSOCIATIVITY_RIGHT: Int = 2
  val NIL: Expression = new ConstantExpression(Constant.apply(LNil.NIL), -1)

  def makeCONCAT(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("..", left, right, PRECEDENCE_CONCAT, ASSOCIATIVITY_RIGHT)
  }

  def makeADD(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("+", left, right, PRECEDENCE_ADD, ASSOCIATIVITY_LEFT)
  }

  def makeSUB(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("-", left, right, PRECEDENCE_ADD, ASSOCIATIVITY_LEFT)
  }

  def makeMUL(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("*", left, right, PRECEDENCE_MUL, ASSOCIATIVITY_LEFT)
  }

  def makeDIV(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("/", left, right, PRECEDENCE_MUL, ASSOCIATIVITY_LEFT)
  }

  def makeMOD(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("%", left, right, PRECEDENCE_MUL, ASSOCIATIVITY_LEFT)
  }

  def makeUNM(expression: Expression): UnaryExpression = {
    new UnaryExpression("-", expression, PRECEDENCE_UNARY)
  }

  def makeNOT(expression: Expression): UnaryExpression = {
    new UnaryExpression("not ", expression, PRECEDENCE_UNARY)
  }

  def makeLEN(expression: Expression): UnaryExpression = {
    new UnaryExpression("#", expression, PRECEDENCE_UNARY)
  }

  def makePOW(left: Expression, right: Expression): BinaryExpression = {
    new BinaryExpression("^", left, right, PRECEDENCE_POW, ASSOCIATIVITY_RIGHT)
  }

  /**
    * Prints out a sequences of expressions with commas, and optionally
    * handling multiple expressions and return value adjustment.
    */
  def printSequence(out: Output, exprs: Seq[Expression], linebreak: Boolean, multiple: Boolean) {
    val n: Int = exprs.size
    var i: Int = 1
    var seenLast:Boolean = false
    for (expr <- exprs; if i <= n && !seenLast) {
      val last: Boolean = i == n || expr.isMultiple
      if (expr.isMultiple) seenLast = true
      if (last) {
        if (multiple) {
          expr.printMultiple(out)
        }
        else {
          expr.print(out)
        }
      }
      else {
        expr.print(out)
        out.print(",")
        if (linebreak) {
          out.println()
        }
        else {
          out.print(" ")
        }
      }
      i += 1
    }
  }

  protected def printUnary(out: Output, op: String, expression: Expression) {
    out.print(op)
    expression.print(out)
  }

  protected def printBinary(out: Output, op: String, left: Expression, right: Expression) {
    left.print(out)
    out.print(" ")
    out.print(op)
    out.print(" ")
    right.print(out)
  }
}

abstract class Expression(val precedence: Int) {


  def print(out: Output)

  /**
    * Prints the expression in a context that accepts multiple values.
    * (Thus, if an expression that normally could return multiple values
    * doesn't, it should use parens to adjust to 1.)
    */
  def printMultiple(out: Output) {
    print(out)
  }

  /**
    * Determines the index of the last-declared constant in this expression.
    * If there is no constant in the expression, return -1.
    */
  def getConstantIndex: Int

  def beginsWithParen: Boolean = {
    false
  }

  def isNil: Boolean = {
    false
  }

  def isClosure: Boolean = {
    false
  }

  def isConstant: Boolean = {
    false
  }

  def isUpvalueOf(register: Int): Boolean = {
    throw new IllegalStateException
  }

  def isBoolean: Boolean = {
    false
  }

  def isInteger: Boolean = {
    false
  }

  def asInteger: Int = {
    throw new IllegalStateException
  }

  def isString: Boolean = {
    false
  }

  def isIdentifier: Boolean = {
    false
  }

  /**
    * Determines if this can be part of a function name.
    * Is it of the form: {Name . } Name
    */
  def isDotChain: Boolean = {
    false
  }

  def closureUpvalueLine: Int = {
    throw new IllegalStateException
  }

  def printClosure(out: Output, name: Target) {
    throw new IllegalStateException
  }

  def asName: String = {
    throw new IllegalStateException
  }

  def isTableLiteral: Boolean = {
    false
  }

  def addEntry(entry: TableLiteral.Entry) {
    throw new IllegalStateException
  }

  /**
    * Whether the expression has more than one return stored into registers.
    */
  def isMultiple: Boolean = {
    false
  }

  def isMemberAccess: Boolean = {
    false
  }

  def getTable: Expression = {
    throw new IllegalStateException
  }

  def getField: String = {
    throw new IllegalStateException
  }

  def isBrief: Boolean = {
    false
  }
}