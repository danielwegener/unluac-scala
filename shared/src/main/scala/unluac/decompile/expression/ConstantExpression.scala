package unluac.decompile.expression

import unluac.decompile.{Constant, Output}

class ConstantExpression(val constant: Constant, val index: Int) extends Expression(Expression.PRECEDENCE_ATOMIC) {

  def getConstantIndex: Int = {
    index
  }

  def print(out: Output) {
    constant.print(out)
  }

  override def isConstant: Boolean = {
    true
  }

  override def isNil: Boolean = {
    constant.isNil
  }

  override def isBoolean: Boolean = {
    constant.isBoolean
  }

  override def isInteger: Boolean = {
    constant.isInteger
  }

  override def asInteger: Int = {
    constant.asInteger
  }

  override def isString: Boolean = {
    constant.isString
  }

  override def isIdentifier: Boolean = {
    return constant.isIdentifier
  }

  override def asName: String = {
    return constant.asName
  }

  override def isBrief: Boolean = {
    return !constant.isString || constant.asName.length <= 10
  }
}