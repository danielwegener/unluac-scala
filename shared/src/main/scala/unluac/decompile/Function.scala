package unluac.decompile

import unluac.decompile.expression.{ConstantExpression, GlobalExpression}
import unluac.parse.LFunction

case class Function(constants: Array[Constant]) {

  def this(function: LFunction) {
    this(function.constants.map(Constant.apply))
  }

  def getGlobalName(constantIndex: Int): String = {
    constants(constantIndex).asName
  }

  def getConstantExpression(constantIndex: Int): ConstantExpression = {
    new ConstantExpression(constants(constantIndex), constantIndex)
  }

  def getGlobalExpression(constantIndex: Int): GlobalExpression = {
    new GlobalExpression(getGlobalName(constantIndex), constantIndex)
  }
}