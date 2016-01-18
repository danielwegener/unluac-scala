package unluac.decompile

import unluac.decompile.expression.UpvalueExpression
import unluac.parse.LUpvalue

case class Upvalues(upvalues: Array[LUpvalue]) {


  def getName(index: Int): String = {
    if (index < upvalues.length && upvalues(index).name != null) {
      upvalues(index).name
    }
    else {
      //TODO: SET ERROR
      "_UPVALUE" + index + "_"
    }
  }

  def getExpression(index: Int): UpvalueExpression = {
    new UpvalueExpression(getName(index))
  }
}