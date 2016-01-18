package unluac.decompile.target

import unluac.decompile.Output

case class UpvalueTarget(name:String) extends Target {

  def print(out: Output) {
    out.print(name)
  }

  def printMethod(out: Output) {
    throw new IllegalStateException
  }
}