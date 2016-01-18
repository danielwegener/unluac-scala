package unluac.decompile

import java.util

import unluac.decompile.expression.{Expression, LocalVariable}
import unluac.decompile.target.{Target, VariableTarget}

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Registers {
  var registers: Int = 0
  var length: Int = 0
  private final var decls: Array[Array[Declaration]] = null
  private final var f: Function = null
  private final var values: Array[Array[Expression]] = null
  private final var updated: Array[Array[Int]] = null

  def this(registers: Int, length: Int, declList: Array[Declaration], f: Function) {
    this()
    this.registers = registers
    this.length = length
    decls = Array.ofDim[Declaration](registers, length + 1)
    for (decl <- declList) {
      var register: Int = 0
      while (decls(register)(decl.begin) != null) {
        register += 1
      }
      decl.register_$eq(register)

      var line: Int = decl.begin
      while (line <= decl.end) {
          decls(register)(line) = decl
          line += 1
      }

    }
    values = Array.ofDim[Expression](registers, length + 1)
      var register: Int = 0
      while (register < registers) {
          values(register)(0) = Expression.NIL
          register += 1
      }
    updated = Array.ofDim(registers, length + 1)
    startedLines = Array.fill(length + 1)(false)

    this.f = f
  }

  def isAssignable(register: Int, line: Int): Boolean = {
    isLocal(register, line) && !decls(register)(line).forLoop
  }

  def isLocal(register: Int, line: Int): Boolean = {
    register >= 0 && decls(register)(line) != null
  }

  def isNewLocal(register: Int, line: Int): Boolean = {
    val decl: Declaration = decls(register)(line)
    decl != null && decl.begin == line && !decl.forLoop
  }

  def getNewLocals(line: Int): Seq[Declaration] = {
    val locals: mutable.Buffer[Declaration] = new ArrayBuffer[Declaration](registers)
      var register: Int = 0
      while (register < registers) {
        {
          if (isNewLocal(register, line)) {
            locals += getDeclaration(register, line)
          }
          register += 1
        }
      }
     locals
  }

  def getDeclaration(register: Int, line: Int): Declaration = {
    decls(register)(line)
  }

  private var startedLines: Array[Boolean] = null

  def startLine(line: Int) {
    startedLines(line) = true

      var register: Int = 0
      while (register < registers) {
        {
          values(register)(line) = values(register)(line - 1)
          updated(register)(line) = updated(register)(line - 1)
          register += 1
        }
      }

  }

  def getExpression(register: Int, line: Int): Expression = {
    if (isLocal(register, line - 1)) {
      new LocalVariable(getDeclaration(register, line - 1))
    }
    else {
      values(register)(line - 1)
    }
  }

  def getKExpression(register: Int, line: Int): Expression = {
    if ((register & 0x100) != 0) {
      f.getConstantExpression(register & 0xFF)
    }
    else {
      getExpression(register, line)
    }
  }

  def getValue(register: Int, line: Int): Expression = {
    values(register)(line - 1)
  }

  def getUpdated(register: Int, line: Int): Int = {
    updated(register)(line)
  }

  def setValue(register: Int, line: Int, expression: Expression) {
    values(register)(line) = expression
    updated(register)(line) = line
  }

  def getTarget(register: Int, line: Int): Target = {
    if (!isLocal(register, line)) {
      throw new IllegalStateException("No declaration exists in register " + register + " at line " + line)
    }
    new VariableTarget(decls(register)(line))
  }

  def setInternalLoopVariable(register: Int, begin: Int, end: Int) {
    var decl: Declaration = getDeclaration(register, begin)
    if (decl == null) {
      decl = new Declaration("_FOR_", begin, end)
      decl.register_$eq(register)
      newDeclaration(decl, register, begin, end)
    }
    decl.forLoop_$eq(true)
  }

  def setExplicitLoopVariable(register: Int, begin: Int, end: Int) {
    var decl: Declaration = getDeclaration(register, begin)
    if (decl == null) {
      decl = new Declaration("_FORV_" + register + "_", begin, end)
      decl.register_$eq(register)
      newDeclaration(decl, register, begin, end)
    }
    decl.forLoopExplicit_$eq(true)
  }

  private def newDeclaration(decl: Declaration, register: Int, begin: Int, end: Int) {
      var line: Int = begin
      while (line <= end) {
        {
          decls(register)(line) = decl
          line += 1
        }
      }
  }
}