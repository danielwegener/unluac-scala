package unluac.decompile.branch

import unluac.decompile.Registers
import unluac.decompile.expression.Expression

abstract class Branch(val line: Int, var begin: Int, var end: Int) {
  var isSet: Boolean = false
  var isCompareSet: Boolean = false
  var isTest: Boolean = false
  var setTarget: Int = -1
  def invert: Branch
  def getRegister: Int
  def asExpression(r: Registers): Expression
  def useExpression(expression: Expression)
}