package unluac.decompile

import unluac.decompile.block._
import unluac.decompile.branch._
import unluac.decompile.expression.{ClosureExpression, Vararg, _}
import unluac.decompile.operation.{CallOperation, ReturnOperation, _}
import unluac.decompile.statement.{Assignment, Statement}
import unluac.decompile.target._
import unluac.parse.{LBoolean, LFunction}
import unluac.util.Stack

import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}

class Decompiler(val function: LFunction) {

  final private val registers: Int = function.maximumStackSize
  final private val length: Int = function.code.length
  final val code: Code = new Code(function)
  final var declList: Array[Declaration] = if (function.locals.length >= function.numParams) {
    val b = new Array[Declaration](function.locals.length)
    var i: Int = 0
    while (i < b.length) {
      b(i) = new Declaration(function.locals(i))
      i += 1
    }
    b
  } else {
    val b = new Array[Declaration](function.numParams)
    var i: Int = 0
    while (i < b.length) {
      b(i) = new Declaration("_ARG_" + i + "_", 0, length - 1)
      i += 1
    }
    b
  }

  final private val functions: Array[LFunction] = function.functions
  final private val params: Int = function.numParams
  final private val vararg: Int = function.vararg
  final private val tforTarget: Op = function.header.version.getTForTarget

  def decompile(): Block = {
    val f: Function = new Function(function)
    val r: Registers = new Registers(registers, length, declList, f)
    val reverseTargets: Array[Boolean] = findReverseTargets(length, code)
    handleBranches(r, first = true, reverseTargets)
    val outer: Block = handleBranches(r, first = false, reverseTargets)
    processSequence(this, blocks, code, declList, function, functions, registers, tforTarget, r, f, 1, length)
    outer
  }

  def print(block: Block) {
    print(new Output, block)
  }

  def print(out: OutputProvider, block: Block) {
    print(new Output(out), block)
  }

  def print(out: Output, block: Block) {
    handleInitialDeclares(out, declList, params, vararg)
    block.print(out)
  }

  def handleInitialDeclares(out: Output, declarations: Array[Declaration], params: Int, vararg: Int) {
    val initdecls: ArrayBuffer[Declaration] = new ArrayBuffer[Declaration](declarations.length)
    var i: Int = params + (vararg & 1)
    while (i < declarations.length) {
      if (declarations(i).begin == 0) {
        initdecls.$plus$eq(declarations(i))
      }
      i += 1
    }
    if (initdecls.nonEmpty) {
      out.print("local ")
      out.print(initdecls.apply(0).name)
      var i: Int = 1
      while (i < initdecls.size) {
        out.print(", ")
        out.print(initdecls.apply(i).name)
        i += 1
      }
      out.println()
    }
  }

  private[decompile] def processLine(code: Code, declList: Array[Declaration], f1: Function, function1: LFunction, functions: Array[LFunction], r: Registers, registers: Int, skip: Array[Boolean], upvalues: Upvalues, line: Int): Seq[Operation] = {
    var A: Int = code.A(line)
    var B: Int = code.B(line)
    var C: Int = code.C(line)
    val Bx: Int = code.Bx(line)
    code.op(line) match {
      case Op.MOVE =>
        Seq(new RegisterSet(line, A, r.getExpression(B, line)))
      case Op.LOADK =>
        Seq(new RegisterSet(line, A, f1.getConstantExpression(Bx)))
      case Op.LOADBOOL =>
        Seq(new RegisterSet(line, A, new ConstantExpression(Constant.apply(if (B != 0) LBoolean.LTRUE
        else LBoolean.LFALSE), -1)))
      case Op.LOADNIL =>
        val maximum:Int = if (function1.header.version.usesOldLoadNilEncoding) B else A + B
        val operations: mutable.Buffer[Operation] = new ArrayBuffer[Operation]
        while (A <= maximum) {
          operations += new RegisterSet(line, A, Expression.NIL)
          A += 1
        }
        operations
      case Op.GETUPVAL =>
        Seq(new RegisterSet(line, A, upvalues.getExpression(B)))
      case Op.GETTABUP =>
        var registerSet: RegisterSet = null
        if (B == 0 && (C & 0x100) != 0) {
          registerSet = new RegisterSet(line, A, f1.getGlobalExpression(C & 0xFF))
        }
        else {
          registerSet = new RegisterSet(line, A, new TableReference(upvalues.getExpression(B), r.getKExpression(C, line)))
        }
        Seq(registerSet)
      case Op.GETGLOBAL =>
        Seq(new RegisterSet(line, A, f1.getGlobalExpression(Bx)))
      case Op.GETTABLE =>
        Seq(new RegisterSet(line, A, new TableReference(r.getExpression(B, line), r.getKExpression(C, line))))
      case Op.SETUPVAL =>
        Seq(new UpvalueSet(line, upvalues.getName(B), r.getExpression(A, line)))
      case Op.SETTABUP =>
        if (A == 0 && (B & 0x100) != 0)
          Seq(new GlobalSet(line, f1.getGlobalName(B & 0xFF), r.getKExpression(C, line)))
        else
          Seq(new TableSet(line, upvalues.getExpression(A), r.getKExpression(B, line), r.getKExpression(C, line), true, line))

      case Op.SETGLOBAL =>
        Seq(new GlobalSet(line, f1.getGlobalName(Bx), r.getExpression(A, line)))
      case Op.SETTABLE =>
        Seq(new TableSet(line, r.getExpression(A, line), r.getKExpression(B, line), r.getKExpression(C, line), true, line))
      case Op.NEWTABLE =>
        Seq(new RegisterSet(line, A, new TableLiteral(B, C)))
      case Op.SELF =>
        val common: Expression = r.getExpression(B, line)
        Seq(new RegisterSet(line, A + 1, common), new RegisterSet(line, A, new TableReference(common, r.getKExpression(C, line))))
      case Op.ADD =>
        Seq(new RegisterSet(line, A, Expression.makeADD(r.getKExpression(B, line), r.getKExpression(C, line))))
      case Op.SUB =>
        Seq(new RegisterSet(line, A, Expression.makeSUB(r.getKExpression(B, line), r.getKExpression(C, line))))
      case Op.MUL =>
        Seq(new RegisterSet(line, A, Expression.makeMUL(r.getKExpression(B, line), r.getKExpression(C, line))))
      case Op.DIV =>
        Seq(new RegisterSet(line, A, Expression.makeDIV(r.getKExpression(B, line), r.getKExpression(C, line))))
      case Op.MOD =>
        Seq(new RegisterSet(line, A, Expression.makeMOD(r.getKExpression(B, line), r.getKExpression(C, line))))
      case Op.POW =>
        Seq(new RegisterSet(line, A, Expression.makePOW(r.getKExpression(B, line), r.getKExpression(C, line))))
      case Op.UNM =>
        Seq(new RegisterSet(line, A, Expression.makeUNM(r.getExpression(B, line))))
      case Op.NOT =>
        Seq(new RegisterSet(line, A, Expression.makeNOT(r.getExpression(B, line))))
      case Op.LEN =>
        Seq(new RegisterSet(line, A, Expression.makeLEN(r.getExpression(B, line))))
      case Op.CONCAT =>
        var value: Expression = r.getExpression(C, line)
        while ( {
          C -= 1
          C + 1
        } > B) {
          value = Expression.makeCONCAT(r.getExpression(C, line), value)
        }
        Seq(new RegisterSet(line, A, value))
      case Op.JMP
           | Op.EQ
           | Op.LT
           | Op.LE
           | Op.TEST
           | Op.TESTSET =>
        Seq.empty
      case Op.CALL =>
        val multiple: Boolean = C >= 3 || C == 0
        if (B == 0) B = registers - A
        if (C == 0) C = registers - A + 1
        val function: Expression = r.getExpression(A, line)
        val arguments: Array[Expression] = new Array[Expression](B - 1)
        var register: Int = A + 1
        while (register <= A + B - 1) {
          arguments(register - A - 1) = r.getExpression(register, line)
          register += 1
        }
        val value: FunctionCall = new FunctionCall(function, arguments, multiple)
        val operations: mutable.Buffer[Operation] = new ArrayBuffer[Operation]
        if (C == 1) {
          operations += new CallOperation(line, value)
        }
        else {
          if (C == 2 && !multiple) {
            operations += new RegisterSet(line, A, value)
          }
          else {
            var register: Int = A
            while (register <= A + C - 2) {
              operations += new RegisterSet(line, register, value)
              register += 1
            }
          }
        }
        operations
      case Op.TAILCALL =>
        if (B == 0) B = registers - A
        val function: Expression = r.getExpression(A, line)
        val arguments: Array[Expression] = new Array[Expression](B - 1)
        var register: Int = A + 1
        while (register <= A + B - 1) {
          arguments(register - A - 1) = r.getExpression(register, line)
          register += 1
        }
        val value: FunctionCall = new FunctionCall(function, arguments, true)
        skip(line + 1) = true
        Seq(new ReturnOperation(line, value))
      case Op.RETURN =>
        if (B == 0) B = registers - A + 1
        val values: Array[Expression] = new Array[Expression](B - 1)
        var register: Int = A
        while (register <= A + B - 2) {
          values(register - A) = r.getExpression(register, line)
          register += 1
        }
        Seq(new ReturnOperation(line, values))
      case Op.FORLOOP
           | Op.FORPREP
           | Op.TFORCALL
           | Op.TFORLOOP =>
        Seq.empty
      case Op.SETLIST =>
        if (C == 0) {
          C = code.codepoint(line + 1)
          skip(line + 1) = true
        }
        if (B == 0) {
          B = registers - A - 1
        }
        val table: Expression = r.getValue(A, line)
        val operations: mutable.Buffer[Operation] = new ArrayBuffer[Operation](B)
        var i: Int = 1
        while (i <= B) {
          operations += new TableSet(line, table, new ConstantExpression(new Constant((C - 1) * 50 + i), -1), r.getExpression(A + i, line), false, r.getUpdated(A + i, line))
          i += 1
        }
        operations

      case Op.CLOSE =>
        Seq.empty
      case Op.CLOSURE =>
        val f: LFunction = functions(Bx)
        if (function1.header.version.usesInlineUpvalueDeclarations) {
          var i: Int = 0
          while (i < f.numUpValues) {
            skip(line + 1 + i) = true
            i += 1
          }
        }
        Seq(new RegisterSet(line, A, new ClosureExpression(f, declList, line + 1)))
      case Op.VARARG =>
        val multiple: Boolean = B != 2
        if (B == 1) throw new IllegalStateException
        if (B == 0) B = registers - A + 1
        val value: Expression = new Vararg(B - 1, multiple)
        val operations: mutable.Buffer[Operation] = new ArrayBuffer[Operation]
        var register: Int = A
        while (register <= A + B - 2) {
          operations += new RegisterSet(line, register, value)
          register += 1
        }
        operations
      case _ =>
        throw new IllegalStateException("Illegal instruction: " + code.op(line))
    }
  }

  def findReverseTargets(length: Int, code: Code): Array[Boolean] = {
    val reverseTarget: Array[Boolean] = new Array[Boolean](length + 1)
    java.util.Arrays.fill(reverseTarget, false)
    var line: Int = 1
    while (line <= length) {
      if ((code.op(line) == Op.JMP )&& code.sBx(line) < 0) {
        reverseTarget(line + 1 + code.sBx(line)) = true
      }
      line += 1
    }
    reverseTarget
  }

  def processOperation(code: Code, f: Function, r: Registers, skip: Array[Boolean], upvalues: Upvalues, operation: Operation, line: Int, _nextLine: Int, block: Block): Assignment = {
    var nextLine: Int = _nextLine
    var assign: Assignment = null
    var wasMultiple: Boolean = false
    val stmt: Statement = operation.process(r, block)
    if (stmt != null) {
      stmt match {
        case assign1: Assignment =>
          assign = assign1
          if (!assign.getFirstValue.isMultiple) {
            block.addStatement(stmt)
          }
          else {
            wasMultiple = true
          }
        case _ =>
          block.addStatement(stmt)
      }
      if (assign != null) {
        while (nextLine < block.end && isMoveIntoTarget(code, r, nextLine)) {
          {
            val target: Target = getMoveIntoTargetTarget(code, r, upvalues, f, nextLine, line + 1)
            val value: Expression = getMoveIntoTargetValue(code, r, nextLine, line + 1)
            assign.addFirst(target, value)
            skip(nextLine) = true
            nextLine += 1
          }
        }
        if (wasMultiple && !assign.getFirstValue.isMultiple) {
          block.addStatement(stmt)
        }
      }
    }
    assign
  }

  def processSequence(decompiler: Decompiler, blocks: mutable.Buffer[Block], code: Code, declList: Array[Declaration], function: LFunction, functions: Array[LFunction], registers: Int, tforTarget: Op, r: Registers, f: Function, begin: Int, end: Int) {
    var blockIndex: Int = 1
    val blockStack: Stack[Block] = new Stack[Block]
    blockStack.push(blocks.head)
    val skip: Array[Boolean] = new Array[Boolean](end + 1)
    var line: Int = begin
    while (line <= end) {
      {
        var forContinued: Boolean = false
        var blockHandler: Operation = null
        var broken: Boolean = false
        while (blockStack.peek.end <= line && !broken) {
          {
            val block: Block = blockStack.pop
            blockHandler = block.process(decompiler)
            if (blockHandler != null) {
              broken = true
            }
          }
        }
        if (blockHandler == null) {
          while (blockIndex < blocks.size && blocks(blockIndex).begin <= line) {
            {
              blockStack.push(blocks({
                blockIndex += 1
                blockIndex - 1
              }))
            }
          }
        }
        val block: Block = blockStack.peek
        r.startLine(line)
        if (skip(line)) {
          val newLocals: Seq[Declaration] = r.getNewLocals(line)
          if (newLocals.nonEmpty) {
            val assign: Assignment = new Assignment
            assign.declare(newLocals.head.begin)
            for (decl <- newLocals) {
              assign.addLast(new VariableTarget(decl), r.getValue(decl.register, line))
            }
            blockStack.peek.addStatement(assign)
          }
          forContinued = true
        }
        if (!forContinued) {
          val upvalues: Upvalues = new Upvalues(function.upvalues)
          val operations: Seq[Operation] = processLine(code, declList, f, function, functions, r, registers, skip, upvalues, line)
          val newLocals: Seq[Declaration] = r.getNewLocals(if (blockHandler == null) line
          else line - 1)
          var assign: Assignment = null
          if (blockHandler == null) {
            if (code.op(line) eq Op.LOADNIL) {
              assign = new Assignment
              var count: Int = 0
              for (operation <- operations) {
                val set: RegisterSet = operation.asInstanceOf[RegisterSet]
                operation.process(r, block)
                if (r.isAssignable(set.register, set.line)) {
                  assign.addLast(r.getTarget(set.register, set.line), set.value)
                  count += 1
                }
              }
              if (count > 0) {
                block.addStatement(assign)
              }
            }
            else {
              for (operation <- operations) {
                val temp: Assignment = processOperation(code, f, r, skip, upvalues, operation, line, line + 1, block)
                if (temp != null) {
                  assign = temp
                }
              }
              if (assign != null && assign.getFirstValue.isMultiple) {
                block.addStatement(assign)
              }
            }
          }
          else {
            assign = processOperation(code, f, r, skip, upvalues, blockHandler, line, line, block)
          }
          if (assign != null) {
            if (newLocals.nonEmpty) {
              assign.declare(newLocals.head.begin)
              for (decl <- newLocals) {
                assign.addLast(new VariableTarget(decl), r.getValue(decl.register, line + 1))
              }
            }
          }
          if (blockHandler == null) {
            if (assign == null && newLocals.nonEmpty && (code.op(line) != Op.FORPREP)) {
              if ((code.op(line) != Op.JMP) || (code.op(line + 1 + code.sBx(line)) != tforTarget)) {
                assign = new Assignment
                assign.declare(newLocals.head.begin)
                for (decl <- newLocals) {
                  assign.addLast(new VariableTarget(decl), r.getValue(decl.register, line))
                }
                blockStack.peek.addStatement(assign)
              }
            }
          }
          else {
            line -= 1
          }
        }
      }
      line += 1
    }
  }

  def isMoveIntoTarget(code: Code, r: Registers, line: Int): Boolean =
    code.op(line) match {
      case Op.MOVE =>
        r.isAssignable(code.A(line), line) && !r.isLocal(code.B(line), line)
      case Op.SETUPVAL
           | Op.SETGLOBAL =>
        !r.isLocal(code.A(line), line)
      case Op.SETTABLE =>
        val C: Int = code.C(line)
        (C & 0x100) == 0 && !r.isLocal(C, line)
      case _ =>
        false
    }

  def getMoveIntoTargetTarget(code: Code, r: Registers, upvalues: Upvalues, function: Function, line: Int, previous: Int): Target =
    code.op(line) match {
      case Op.MOVE =>
        r.getTarget(code.A(line), line)
      case Op.SETUPVAL =>
        new UpvalueTarget(upvalues.getName(code.B(line)))
      case Op.SETGLOBAL =>
        new GlobalTarget(function.getGlobalName(code.Bx(line)))
      case Op.SETTABLE =>
        new TableTarget(r.getExpression(code.A(line), previous), r.getKExpression(code.B(line), previous))
      case _ =>
        throw new IllegalStateException
    }

  def getMoveIntoTargetValue(code: Code, r: Registers, line: Int, previous: Int): Expression = {
    val A: Int = code.A(line)
    val B: Int = code.B(line)
    val C: Int = code.C(line)
    code.op(line) match {
      case Op.MOVE =>
        r.getValue(B, previous)
      case Op.SETUPVAL
           | Op.SETGLOBAL =>
        r.getExpression(A, previous)
      case Op.SETTABLE =>
        if ((C & 0x100) != 0) {
          throw new IllegalStateException
        }
        else {
          r.getExpression(C, previous)
        }
      case _ =>
        throw new IllegalStateException
    }
  }

  private var blocks: ArrayBuffer[Block] = null

  private def handleBranches(r: Registers, first: Boolean, reverseTargets: Array[Boolean]): OuterBlock = {
    val oldBlocks: ArrayBuffer[Block] = blocks
    blocks = new ArrayBuffer[Block]
    val outer: OuterBlock = new OuterBlock(function, length)
    blocks += outer
    val isBreak: Array[Boolean] = new Array[Boolean](length + 1)
    val loopRemoved: Array[Boolean] = new Array[Boolean](length + 1)
    if (!first) {
      oldBlocks.foreach(block => {
        if (block.isInstanceOf[AlwaysLoop]) {
          blocks += block
        }
        if (block.isInstanceOf[Break]) {
          blocks += block
          isBreak(block.begin) = true
        }
      })

      val delete: mutable.Buffer[Block] = new ListBuffer[Block]

      blocks.filter(block => block.isInstanceOf[AlwaysLoop]).foreach(block => blocks
        .filter(block2 => block != block2)
        .filter(block2 => block.begin == block2.begin)
        .foreach(block2 => {
          if (block.end < block2.end) {
            delete += block
            loopRemoved(block.end - 1) = true
          } else {
            delete += block2
            loopRemoved(block2.end - 1) = true
          }
        }))
      delete.foreach(block => {
        val idx = blocks.indexOf(block)
        if (idx > -1) blocks.remove(idx)
      })
    }
    val skip: Array[Boolean] = new Array[Boolean](length + 1)
    val stack: Stack[Branch] = new Stack[Branch]
    var reduce: Boolean = false
    var testset: Boolean = false
    var testsetend: Int = -1
    var line: Int = 1
    while (line <= length) {
      {
        var skipReduce: Boolean = false
        if (!skip(line)) {
          code.op(line) match {
            case Op.EQ =>
              val node: EQNode = new EQNode(code.B(line), code.C(line), code.A(line) != 0, line, line + 2, line + 2 + code.sBx(line + 1))
              stack.push(node)
              skip(line + 1) = true
              if (code.op(node.end) eq Op.LOADBOOL) {
                if (code.C(node.end) != 0) {
                  node.isCompareSet_$eq(true)
                  node.setTarget_$eq(code.A(node.end))
                }
                else if (code.op(node.end - 1) eq Op.LOADBOOL) {
                  if (code.C(node.end - 1) != 0) {
                    node.isCompareSet_$eq(true)
                    node.setTarget_$eq(code.A(node.end))
                  }
                }
              }
              skipReduce = true
            case Op.LT =>
              val node: LTNode = new LTNode(code.B(line), code.C(line), code.A(line) != 0, line, line + 2, line + 2 + code.sBx(line + 1))
              stack.push(node)
              skip(line + 1) = true
              if (code.op(node.end) eq Op.LOADBOOL) {
                if (code.C(node.end) != 0) {
                  node.isCompareSet_$eq(true)
                  node.setTarget_$eq(code.A(node.end))
                }
                else if (code.op(node.end - 1) eq Op.LOADBOOL) {
                  if (code.C(node.end - 1) != 0) {
                    node.isCompareSet_$eq(true)
                    node.setTarget_$eq(code.A(node.end))
                  }
                }
              }
              skipReduce = true
            case Op.LE =>
              val node: LENode = new LENode(code.B(line), code.C(line), code.A(line) != 0, line, line + 2, line + 2 + code.sBx(line + 1))
              stack.push(node)
              skip(line + 1) = true
              if (code.op(node.end) eq Op.LOADBOOL) {
                if (code.C(node.end) != 0) {
                  node.isCompareSet_$eq(true)
                  node.setTarget_$eq(code.A(node.end))
                }
                else if (code.op(node.end - 1) eq Op.LOADBOOL) {
                  if (code.C(node.end - 1) != 0) {
                    node.isCompareSet_$eq(true)
                    node.setTarget_$eq(code.A(node.end))
                  }
                }
              }
              skipReduce = true
            case Op.TEST =>
              stack.push(new TestNode(code.A(line), code.C(line) != 0, line, line + 2, line + 2 + code.sBx(line + 1)))
              skip(line + 1) = true
              skipReduce = true
            case Op.TESTSET =>
              testset = true
              testsetend = line + 2 + code.sBx(line + 1)
              stack.push(new TestSetNode(code.A(line), code.B(line), code.C(line) != 0, line, line + 2, line + 2 + code.sBx(line + 1)))
              skip(line + 1) = true
              skipReduce = true
            case Op.JMP =>
              reduce = true
              val tline: Int = line + 1 + code.sBx(line)
              if (tline >= 2 && (code.op(tline - 1) == Op.LOADBOOL) && code.C(tline - 1) != 0) {
                stack.push(new TrueNode(code.A(tline - 1), false, line, line + 1, tline))
                skip(line + 1) = true
              }
              else if ((code.op(tline) == tforTarget) && !skip(tline)) {
                val A: Int = code.A(tline)
                val C: Int = code.C(tline)
                if (C == 0) throw new IllegalStateException
                r.setInternalLoopVariable(A, tline, line + 1)
                r.setInternalLoopVariable(A + 1, tline, line + 1)
                r.setInternalLoopVariable(A + 2, tline, line + 1)
                var index: Int = 1
                while (index <= C) {
                  r.setExplicitLoopVariable(A + 2 + index, line, tline + 2)
                  index += 1
                }
                skip(tline) = true
                skip(tline + 1) = true
                blocks += new TForBlock(function, line + 1, tline + 2, A, C, r)
              }
              else if (code.sBx(line) == 2 && (code.op(line + 1) == Op.LOADBOOL) && code.C(line + 1) != 0) {
                blocks += new BooleanIndicator(function, line)
              }
              else {
                if (first || loopRemoved(line)) {
                  if (tline > line) {
                    isBreak(line) = true
                    blocks += new Break(function, line, tline)
                  }
                  else {
                    val enclosing: Block = enclosingBreakableBlock(blocks, line)
                    if (enclosing != null && enclosing.breakable && (code.op(enclosing.end) == Op.JMP) && code.sBx(enclosing.end) + enclosing.end + 1 == tline) {
                      isBreak(line) = true
                      blocks += new Break(function, line, enclosing.end)
                    }
                    else {
                      blocks += new AlwaysLoop(function, tline, line + 1)
                    }
                  }
                }
              }
            case Op.FORPREP =>
              reduce = true
              blocks += new ForBlock(function, line + 1, line + 2 + code.sBx(line), code.A(line), r)
              skip(line + 1 + code.sBx(line)) = true
              r.setInternalLoopVariable(code.A(line), line, line + 2 + code.sBx(line))
              r.setInternalLoopVariable(code.A(line) + 1, line, line + 2 + code.sBx(line))
              r.setInternalLoopVariable(code.A(line) + 2, line, line + 2 + code.sBx(line))
              r.setExplicitLoopVariable(code.A(line) + 3, line, line + 2 + code.sBx(line))
            case Op.FORLOOP =>
              throw new IllegalStateException
            case _ =>
              reduce = isStatement(r, registers, code, line)
          }
        }
        if (!skipReduce) {
          if ((line + 1) <= length && reverseTargets(line + 1)) {
            reduce = true
          }
          if (testset && testsetend == line + 1) {
            reduce = true
          }
          if (stack.isEmpty) {
            reduce = false
          }
          if (reduce) {
            reduce = false
            val conditions: Stack[Branch] = new Stack[Branch]
            val backups: Stack[Stack[Branch]] = new Stack[Stack[Branch]]
            var firstIteration: Boolean = true
            while (firstIteration || !stack.isEmpty) {
              {
                firstIteration = false
                var isAssignNode: Boolean = stack.peek.isInstanceOf[TestSetNode]
                var assignEnd: Int = stack.peek.end
                var compareCorrect: Boolean = false
                if (stack.peek.isInstanceOf[TrueNode]) {
                  isAssignNode = true
                  compareCorrect = true
                  if (code.C(assignEnd) != 0) {
                    assignEnd += 2
                  }
                  else {
                    assignEnd += 1
                  }
                }
                else if (stack.peek.isCompareSet) {
                  if ((code.op(stack.peek.begin) != Op.LOADBOOL) || code.C(stack.peek.begin) == 0) {
                    isAssignNode = true
                    if (code.C(assignEnd) != 0) {
                      assignEnd += 2
                    }
                    else {
                      assignEnd += 1
                    }
                    compareCorrect = true
                  }
                }
                else if (assignEnd - 3 >= 1 && (code.op(assignEnd - 2) == Op.LOADBOOL) && code.C(assignEnd - 2) != 0 && (code.op(assignEnd - 3) == Op.JMP) && code.sBx(assignEnd - 3) == 2) {
                  stack.peek match {
                    case node: TestNode =>
                      if (node.test == code.A(assignEnd - 2)) {
                        isAssignNode = true
                      }
                    case _ =>
                  }
                }
                else if (assignEnd - 2 >= 1 && (code.op(assignEnd - 1) == Op.LOADBOOL) && code.C(assignEnd - 1) != 0 && (code.op(assignEnd - 2) == Op.JMP) && code.sBx(assignEnd - 2) == 2) {
                  if (stack.peek.isInstanceOf[TestNode]) {
                    isAssignNode = true
                    assignEnd += 1
                  }
                }
                else if(assignEnd - 1 >= 1 && code.op(assignEnd) == Op.LOADBOOL && code.C(assignEnd) != 0 && code.op(assignEnd - 1) == Op.JMP && code.sBx(assignEnd - 1) == 2) {
                  if(stack.peek.isInstanceOf[TestNode]) {
                    isAssignNode = true
                    assignEnd += 2
                  }
                }
                else if (assignEnd - 1 >= 1 && r.isLocal(getAssignment(code, assignEnd - 1), assignEnd - 1) && assignEnd > stack.peek.line) {
                  val decl: Declaration = r.getDeclaration(getAssignment(code, assignEnd - 1), assignEnd - 1)
                  if (decl.begin == assignEnd - 1 && decl.end > assignEnd - 1) {
                    isAssignNode = true
                  }
                }
                if (!compareCorrect && assignEnd - 1 == stack.peek.begin && (code.op(stack.peek.begin) == Op.LOADBOOL) && code.C(stack.peek.begin) != 0) {
                  backup = null
                  val begin: Int = stack.peek.begin
                  assignEnd = begin + 2
                  val target: Int = code.A(begin)
                  val b: Branch = popCompareSetCondition(code, stack, backup, assignEnd)
                  b.setTarget_$eq(target)
                  b.end_$eq(assignEnd)
                  b.begin_$eq(begin)
                  conditions.push(b)
                }
                else if (isAssignNode) {
                  backup = null
                  val target: Int = stack.peek.setTarget
                  val begin: Int = stack.peek.begin
                  val branch: Branch = popSetCondition(stack, assignEnd)
                  branch.setTarget_$eq(target)
                  branch.end_$eq(assignEnd)
                  branch.begin_$eq(begin)
                  conditions.push(branch)
                }
                else {
                  backup = new Stack[Branch]
                  conditions.push(popCondition(code, stack, backup))
                  backup = backup.reverse()
                }
                backups.push(backup)
              }
            }

            {
              var firstIteration: Boolean = true
              while (firstIteration || !conditions.isEmpty) {
                {
                  firstIteration = false
                  var cond: Branch = conditions.pop
                  val backup: Stack[Branch] = backups.pop
                  var breakTarget: Int = breakTargetF(cond.begin, blocks)
                  val breakable: Boolean = breakTarget >= 1
                  if (breakable && (code.op(breakTarget) == Op.JMP)) {
                    breakTarget += 1 + code.sBx(breakTarget)
                  }
                  if (breakable && breakTarget == cond.end) {
                    val immediateEnclosing: Block = enclosingBlock(blocks, cond.begin)
                    var loopBroken: Boolean = false
                    var iline: Int = Math.max(cond.end, immediateEnclosing.end - 1)
                    while (iline >= Math.max(cond.begin, immediateEnclosing.begin) && !loopBroken) {
                      if ((code.op(iline) == Op.JMP) && iline + 1 + code.sBx(iline) == breakTarget) {
                        cond.end_$eq(iline)
                        loopBroken = true
                      }
                      iline -= 1
                    }
                  }
                  var hasTail: Boolean = cond.end >= 2 && (code.op(cond.end - 1) == Op.JMP)
                  var tail: Int = if (hasTail) cond.end + code.sBx(cond.end - 1)
                  else -1
                  val originalTail: Int = tail
                  val enclosing: Block = enclosingUnprotectedBlock(blocks, cond.begin)
                  if (enclosing != null) {
                    if (enclosing.getLoopback == cond.end) {
                      cond.end_$eq(enclosing.end - 1)
                      hasTail = cond.end >= 2 && (code.op(cond.end - 1) == Op.JMP)
                      tail = if (hasTail) cond.end + code.sBx(cond.end - 1)
                      else -1
                    }
                    if (hasTail && enclosing.getLoopback == tail) {
                      tail = enclosing.end - 1
                    }
                  }
                  if (cond.isSet) {
                    var empty: Boolean = cond.begin == cond.end
                    if ((code.op(cond.begin) == Op.JMP) && code.sBx(cond.begin) == 2 && (code.op(cond.begin + 1) == Op.LOADBOOL) && code.C(cond.begin + 1) != 0) {
                      empty = true
                    }
                    blocks += new SetBlock(function, cond, cond.setTarget, line, cond.begin, cond.end, empty, r)
                  }
                  else if ((code.op(cond.begin) == Op.LOADBOOL) && code.C(cond.begin) != 0) {
                    val begin: Int = cond.begin
                    val target: Int = code.A(begin)
                    if (code.B(begin) == 0) {
                      cond = cond.invert
                    }
                    blocks += new CompareBlock(function, begin, begin + 2, target, cond)
                  }
                  else if (cond.end < cond.begin) {
                    blocks += new RepeatBlock(function, cond, r)
                  }
                  else if (hasTail) {
                    val endOp: Op = code.op(cond.end - 2)
                    val isEndCondJump: Boolean = (endOp == Op.EQ) || (endOp == Op.LE) || (endOp == Op.LT) || (endOp == Op.TEST) || (endOp == Op.TESTSET)
                    if (tail > cond.end || (tail == cond.end && !isEndCondJump)) {
                      val op: Op = code.op(tail - 1)
                      val sbx: Int = code.sBx(tail - 1)
                      val loopback2: Int = tail + sbx
                      val isBreakableLoopEnd: Boolean = function.header.version.isBreakableLoopEnd(op)
                      if (isBreakableLoopEnd && loopback2 <= cond.begin && !isBreak(tail - 1)) {
                        blocks += new IfThenEndBlock(function, cond, backup, r)
                      }
                      else {
                        skip(cond.end - 1) = true
                        val emptyElse: Boolean = tail == cond.end
                        val ifthen: IfThenElseBlock = new IfThenElseBlock(function, cond, originalTail, emptyElse, r)
                        blocks+= ifthen
                        if (!emptyElse) {
                          val elseend: ElseEndBlock = new ElseEndBlock(function, cond.end, tail)
                          blocks += elseend
                        }
                      }
                    }
                    else {
                      var existsStatement: Boolean = false
                      var loopBroken: Boolean = false
                      var sl: Int = tail
                      while (sl < cond.begin && !loopBroken) {
                        if (!skip(sl) && isStatement(r, registers, code, sl)) {
                          existsStatement = true
                          loopBroken = true
                        }
                        sl += 1
                      }
                      if (tail >= cond.begin || existsStatement) {
                        blocks += new IfThenEndBlock(function, cond, backup, r)
                      }
                      else {
                        skip(cond.end - 1) = true
                        blocks += new WhileBlock(function, cond, originalTail, r)
                      }
                    }
                  }
                  else {
                    blocks += new IfThenEndBlock(function, cond, backup, r)
                  }
                }
              }
            }

          }
        }
        line += 1
      }
    }
    for (decl <- declList) {
      if (!decl.forLoop && !decl.forLoopExplicit) {
        val needsDoEnd: Boolean = !blocks.exists(block => block.contains(decl.begin) && block.scopeEnd == decl.end)
        if (needsDoEnd) {
          blocks += new DoEndBlock(function, decl.begin, decl.end + 1)
        }
      }
    }

    blocks = blocks.filterNot(block => skip(block.begin) && block.isInstanceOf[Break]).sorted
    backup = null
    outer
  }

  private def breakTargetF(line: Int, blocks: Seq[Block]): Int = {
    val filtered = blocks.view.filter(block => block.breakable && block.contains(line)).map(_.end)
    if (filtered.isEmpty) -1
    else filtered.min
  }

  private def enclosingBlock(blocks: IndexedSeq[Block], line: Int): Block = {
    var enclosing: Block = blocks(0)
    var i: Int = 1
    while (i < blocks.size) {
      val next: Block = blocks(i)
      if (next.isContainer && enclosing.contains(next) && next.contains(line) && !next.loopRedirectAdjustment) {
        enclosing = next
      }
      i += 1
    }
    enclosing
  }

  private def enclosingBreakableBlock(blocks: mutable.Buffer[Block], line: Int): Block = {
    val outer: Block = blocks.head
    var enclosing: Block = outer
    for (next <- blocks) {
      if (enclosing.contains(next) && next.contains(line) && next.breakable && !next.loopRedirectAdjustment) {
        enclosing = next
      }
    }
    if (enclosing eq outer) null
    else enclosing
  }

  private def enclosingUnprotectedBlock(blocks: IndexedSeq[Block], line: Int): Block = {
    val outer: Block = blocks.head
    var enclosing: Block = outer
    var i: Int = 1
    while (i < blocks.size) {
      val next: Block = blocks(i)
      if (enclosing.contains(next) && next.contains(line) && next.isUnprotected && !next.loopRedirectAdjustment) {
        enclosing = next
      }
      i += 1
    }
    if (enclosing eq outer) null
    else enclosing
  }

  private var backup: Stack[Branch] = null

  def popCondition(code: Code, stack: Stack[Branch], backup: Stack[Branch]): Branch = {
    var branch: Branch = stack.pop
    if (backup != null) backup.push(branch)
    if (branch.isInstanceOf[TestSetNode]) {
      throw new IllegalStateException
    }
    var begin: Int = branch.begin
    if (code.op(branch.begin) eq Op.JMP) {
      begin += 1 + code.sBx(branch.begin)
    }
    var whileBroken: Boolean = false
    while (!stack.isEmpty && !whileBroken) {
      {
        val next: Branch = stack.peek
        if (next.isInstanceOf[TestSetNode]) {
          whileBroken = true
        }
        if (!whileBroken) {
          if (next.end == begin) {
            branch = new OrBranch(popCondition(code, stack, backup).invert, branch)
          }
          else if (next.end == branch.end) {
            branch = new AndBranch(popCondition(code, stack, backup), branch)
          }
          else {
            whileBroken = true
          }
        }
      }
    }
    branch
  }

  def popSetCondition(stack: Stack[Branch], assignEnd: Int): Branch = {
    stack.push(new AssignNode(assignEnd - 1, assignEnd, assignEnd))
    _helper_popSetCondition(code, stack, backup, invert = false, assignEnd)
  }

  def popCompareSetCondition(code: Code, stack: Stack[Branch], backup: Stack[Branch], assignEnd: Int): Branch = {
    val top: Branch = stack.pop
    var invert: Boolean = false
    if (code.B(top.begin) == 0) invert = true
    top.begin_$eq(assignEnd)
    top.end_$eq(assignEnd)
    stack.push(top)
    _helper_popSetCondition(code, stack, backup, invert, assignEnd)
  }

  private def _helper_popSetCondition(code: Code, stack: Stack[Branch], backup: Stack[Branch], invert: Boolean, assignEnd: Int): Branch = {
    var branch: Branch = stack.pop
    var begin: Int = branch.begin
    var end: Int = branch.end
    if (invert) {
      branch = branch.invert
    }
    if (code.op(begin) eq Op.LOADBOOL) {
      if (code.C(begin) != 0) {
        begin += 2
      } else {
        begin += 1
      }
    }
    if (code.op(end) eq Op.LOADBOOL) {
      if (code.C(end) != 0) {
        end += 2
      } else {
        end += 1
      }
    }
    val target: Int = branch.setTarget
    var whileBroken: Boolean = false
    while (!stack.isEmpty && !whileBroken) {
      {
        val next: Branch = stack.peek
        var ninvert: Boolean = false
        var nend: Int = next.end
        if (code.op(next.end) eq Op.LOADBOOL) {
          ninvert = code.B(next.end) != 0
          if (code.C(next.end) != 0) {
            nend += 2
          }
          else {
            nend += 1
          }
        } else next match {
          case node: TestSetNode =>
            ninvert = node.inverted
          case node: TestNode =>
            ninvert = node.inverted
          case _ =>
            ninvert = false
            if (nend >= assignEnd) {
              whileBroken = true
            }
        }
        if (!whileBroken) {
          var addr: Int = 0
          if (ninvert == invert) {
            addr = end
          } else {
            addr = begin
          }
          if (addr == nend) {
            if (ninvert) {
              branch = new OrBranch(_helper_popSetCondition(code, stack, backup, invert = true, assignEnd), branch)
            } else {
              branch = new AndBranch(_helper_popSetCondition(code, stack, backup, invert = false, assignEnd), branch)
            }
            branch.end_$eq(nend)
          } else {
            if (!branch.isInstanceOf[TestSetNode]) {
              stack.push(branch)
              branch = popCondition(code, stack, backup)
            }
            whileBroken = true
          }
        }
      }
    }
    branch.isSet_$eq(true)
    branch.setTarget_$eq(target)
    branch
  }

  private def isStatement(r: Registers, registers: Int, code: Code, line: Int): Boolean = {
    isStatement(r, registers, code, line, -1)
  }

  private def isStatement(r: Registers, registers: Int, code: Code, line: Int, testRegister: Int): Boolean = {
    code.op(line) match {
      case Op.MOVE
           | Op.LOADK
           | Op.LOADBOOL
           | Op.GETUPVAL
           | Op.GETTABUP
           | Op.GETGLOBAL
           | Op.GETTABLE
           | Op.NEWTABLE
           | Op.ADD
           | Op.SUB
           | Op.MUL
           | Op.DIV
           | Op.MOD
           | Op.POW
           | Op.UNM
           | Op.NOT
           | Op.LEN
           | Op.CONCAT
           | Op.CLOSURE  =>
        r.isLocal(code.A(line), line) || code.A(line) == testRegister
      case Op.LOADNIL =>
        var register: Int = code.A(line)
        while (register <= code.B(line)) {
          {
            if (r.isLocal(register, line)) {
              return true
            }
            register += 1
          }
        }
        false
      case Op.SETGLOBAL
           | Op.SETUPVAL
           | Op.SETTABUP
           | Op.SETTABLE
           | Op.JMP
           | Op.TAILCALL
           | Op.RETURN
           | Op.FORLOOP
           | Op.FORPREP
           | Op.TFORCALL
           | Op.TFORLOOP
           | Op.CLOSE =>
        true
      case Op.SELF =>
        r.isLocal(code.A(line), line) || r.isLocal(code.A(line) + 1, line)
      case Op.EQ
           | Op.LT
           | Op.LE
           | Op.TEST
           | Op.TESTSET
           | Op.SETLIST =>
        false
      case Op.CALL =>
        val a: Int = code.A(line)
        var c: Int = code.C(line)
        if (c == 1) {
          return true
        }
        if (c == 0) c = registers - a + 1
        var register: Int = a
        while (register < a + c - 1) {
          if (r.isLocal(register, line)) {
            return true
          }
          register += 1
        }
        c == 2 && a == testRegister

      case Op.VARARG =>
        val a: Int = code.A(line)
        var b: Int = code.B(line)
        if (b == 0) b = registers - a + 1
        var register: Int = a
        while (register < a + b - 1) {
          if (r.isLocal(register, line)) {
            return true
          }
          register += 1
        }
        false

      case _ =>
        throw new IllegalStateException("Illegal opcode: " + code.op(line))
    }
  }

  /**
    * Returns the single register assigned to at the line or
    * -1 if no register or multiple registers is/are assigned to.
    */
  def getAssignment(code: Code, line: Int): Int = {
    code.op(line) match {
      case Op.MOVE
           | Op.LOADK
           | Op.LOADBOOL
           | Op.GETUPVAL
           | Op.GETTABUP
           | Op.GETGLOBAL
           | Op.GETTABLE
           | Op.NEWTABLE
           | Op.ADD
           | Op.SUB
           | Op.MUL
           | Op.DIV
           | Op.MOD
           | Op.POW
           | Op.UNM
           | Op.NOT
           | Op.LEN
           | Op.CONCAT
           | Op.CLOSURE =>
        code.A(line)
      case Op.LOADNIL =>
        if (code.A(line) == code.B(line)) {
          code.A(line)
        }
        else {
          -1
        }
      case Op.SETGLOBAL
           | Op.SETUPVAL
           | Op.SETTABUP
           | Op.SETTABLE
           | Op.JMP
           | Op.TAILCALL
           | Op.RETURN
           | Op.FORLOOP
           | Op.FORPREP
           | Op.TFORCALL
           | Op.TFORLOOP
           | Op.CLOSE =>
        -1
      case Op.SELF =>
        -1
      case Op.EQ
           | Op.LT
           | Op.LE
           | Op.TEST
           | Op.TESTSET
           | Op.SETLIST =>
        -1
      case Op.CALL =>
        if (code.C(line) == 2) {
          code.A(line)
        } else {
          -1
        }
      case Op.VARARG =>
        if (code.C(line) == 2) {
          code.B(line)
        }
        else {
          -1
        }
      case _ =>
        throw new IllegalStateException("Illegal opcode: " + code.op(line))
    }
  }


}