package unluac.decompile.target

import unluac.decompile.Declaration
import unluac.decompile.Output

trait Target {
  def print(out: Output)

  def printMethod(out: Output)

  def isDeclaration(decl: Declaration): Boolean = {
    false
  }

  def isLocal: Boolean = {
    false
  }

  def getIndex: Int = {
    throw new IllegalStateException
  }

  def isFunctionName: Boolean = {
    true
  }
}