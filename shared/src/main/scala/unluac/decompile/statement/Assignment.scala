package unluac.decompile.statement

import java.util

import unluac.decompile.{Declaration, Output}
import unluac.decompile.expression.Expression
import unluac.decompile.target.Target

import scala.collection.mutable.ArrayBuffer

class Assignment extends Statement {
  private final val targets: ArrayBuffer[Target] = new ArrayBuffer[Target](5)
  private final val values: ArrayBuffer[Expression] = new ArrayBuffer[Expression](5)
  private var allnil: Boolean = true
  private var declare: Boolean = false
  private var declareStart: Int = 0

  def this(target: Target, value: Expression) {
    this()
    targets += target
    values += value
    allnil = allnil && value.isNil
  }

  def getFirstTarget: Target = {
    targets(0)
  }

  def getFirstValue: Expression = {
    values(0)
  }

  def assignsTarget(decl: Declaration): Boolean = {
    for (target <- targets) {
      if (target.isDeclaration(decl)) {
        return true
      }
    }
    false
  }

  def getArity: Int = {
    targets.size
  }

  def addFirst(target: Target, value: Expression) {
    targets.insert(0, target)
    values.insert(0, value)
    allnil = allnil && value.isNil
  }

  def addLast(target: Target, _value: Expression) {
    var value = _value
    if (targets.contains(target)) {
      val index: Int = targets.indexOf(target)
      targets.remove(index)
      value = values.remove(index)
    }
    targets += target
    values += value
    allnil = allnil && value.isNil
  }

  def assignListEquals(decls: util.List[Declaration]): Boolean = {
    import scala.util.control.Breaks._
    if (decls.size != targets.size) return false
    breakable(for (target <- targets) {
      var found: Boolean = false
      import scala.collection.JavaConversions._
      for (decl <- decls) {
        if (target.isDeclaration(decl)) {
          found = true
          break //todo: break is not supported
        }
      }
      if (!found) return false
    })
    true
  }

  def declare(declareStart: Int) {
    declare = true
    this.declareStart = declareStart
  }

  def print(out: Output) {
    if (targets.nonEmpty) {
      if (declare) {
        out.print("local ")
      }
      var functionSugar: Boolean = false
      if (targets.size == 1 && values.size == 1 && values(0).isClosure && targets(0).isFunctionName) {
        val closure: Expression = values(0)
        if (!declare || declareStart >= closure.closureUpvalueLine) {
          functionSugar = true
        }
        if (targets(0).isLocal && closure.isUpvalueOf(targets(0).getIndex)) {
          functionSugar = true
        }
      }
      if (!functionSugar) {
        targets(0).print(out)
          var i: Int = 1
          while (i < targets.size) {
              out.print(", ")
              targets(i).print(out)
              i += 1
          }
        if (!declare || !allnil) {
          out.print(" = ")
          Expression.printSequence(out, values, linebreak = false, multiple = false)
        }
      }
      else {
        values(0).printClosure(out, targets(0))
      }
      if (comment != null) {
        out.print(" -- ")
        out.print(comment)
      }
    }
  }
}