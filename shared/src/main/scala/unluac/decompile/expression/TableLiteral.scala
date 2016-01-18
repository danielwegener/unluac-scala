package unluac.decompile.expression

import java.util
import java.util.Collections
import unluac.decompile.Output

object TableLiteral {

  case class Entry(key: Expression, value: Expression, isList: Boolean, timestamp: Int) extends Comparable[TableLiteral.Entry] {

    def compareTo(e: TableLiteral.Entry): Int = Integer.compare(timestamp,e.timestamp)
  }

}

class TableLiteral(arraySize: Int, hashSize: Int) extends Expression(Expression.PRECEDENCE_ATOMIC) {
  private val entries: util.ArrayList[TableLiteral.Entry] = new util.ArrayList[TableLiteral.Entry](arraySize + hashSize)
  private var isObject: Boolean = true
  private var isList: Boolean = true
  private var listLength: Int = 1



  def getConstantIndex: Int = {
    var index: Int = -1
    import scala.collection.JavaConversions._
    for (entry <- entries) {
      index = Math.max(entry.key.getConstantIndex, index)
      index = Math.max(entry.value.getConstantIndex, index)
    }
    index
  }

  def print(out: Output) {
    Collections.sort(entries)
    listLength = 1
    if (entries.isEmpty) {
      out.print("{}")
    }
    else {
      var lineBreak: Boolean = isList && entries.size > 5 || isObject && entries.size > 2 || !isObject
      if (!lineBreak) {
        import scala.collection.JavaConversions._
        var forBreaker = false
        for (entry <- entries; if !forBreaker) {
          val value: Expression = entry.value
          if (!value.isBrief) {
            lineBreak = true
            forBreaker = true
          }
        }
      }
      out.print("{")
      if (lineBreak) {
        out.println()
        out.indent()
      }
      printEntry(0, out)
      if (!entries.get(0).value.isMultiple) {

          var index: Int = 1
          var whileBreaker = false
          while (index < entries.size && !whileBreaker) {
            {
              out.print(",")
              if (lineBreak) {
                out.println()
              }
              else {
                out.print(" ")
              }
              printEntry(index, out)
              if (entries.get(index).value.isMultiple) {
                whileBreaker = true
              } else {
                index += 1
              }
            }
          }

      }
      if (lineBreak) {
        out.println()
        out.dedent()
      }
      out.print("}")
    }
  }

  private def printEntry(index: Int, out: Output) {
    val entry: TableLiteral.Entry = entries.get(index)
    val key: Expression = entry.key
    val value: Expression = entry.value
    val isList: Boolean = entry.isList
    val multiple: Boolean = index + 1 >= entries.size || value.isMultiple
    if (isList && key.isInteger && listLength == key.asInteger) {
      if (multiple) {
        value.printMultiple(out)
      }
      else {
        value.print(out)
      }
      listLength += 1
    }
    else if (isObject && key.isIdentifier) {
      out.print(key.asName)
      out.print(" = ")
      value.print(out)
    }
    else {
      out.print("[")
      key.print(out)
      out.print("] = ")
      value.print(out)
    }
  }

  override def isTableLiteral: Boolean = {
    true
  }

  override def addEntry(entry: TableLiteral.Entry) {
    entries.add(entry)
    isObject = isObject && (entry.isList || entry.key.isIdentifier)
    isList = isList && entry.isList
  }

  override def isBrief: Boolean = {
    false
  }
}