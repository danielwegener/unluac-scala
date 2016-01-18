package unluac.decompile.target

import unluac.decompile.Declaration
import unluac.decompile.Output

case class VariableTarget(decl: Declaration) extends Target {

  def print(out: Output) {
    out.print(decl.name)
  }

  def printMethod(out: Output) {
    throw new IllegalStateException
  }

  override def isDeclaration(decl: Declaration): Boolean = {
    this.decl == decl
  }

  override def isLocal: Boolean = {
    true
  }

  override def getIndex: Int = {
    decl.register
  }

  override def equals(obj: Any): Boolean = {
    obj match {
      case t: VariableTarget =>
        decl == t.decl
      case _ =>
        false
    }
  }
}