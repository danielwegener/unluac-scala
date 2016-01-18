package unluac.decompile

import unluac.parse.{LBoolean, LNil, LNumber, LObject, LString}


object Constant {
  private val reservedWords: Set[String] = Set(
  "and",
  "break",
  "do",
  "else",
  "elseif",
  "end",
  "false",
  "for",
  "function",
  "if",
  "in",
  "local",
  "nil",
  "not",
  "or",
  "repeat",
  "return",
  "then",
  "true",
  "until",
  "while"
  )

  def apply(constant: LObject):Constant = {
    constant match {
      case _: LNil =>
        Constant(0, bool = false, null, null)
      case _: LBoolean =>
        Constant(1, bool = constant == LBoolean.LTRUE, null, null)
      case number: LNumber =>
        Constant(2, bool = false, number, null)
      case string: LString =>
        Constant(3, bool = false, null, string.deref)
      case _ =>
        throw new scala.IllegalArgumentException("Illegal constant type: " + constant.toString)
    }
  }

}

case class Constant(`type`: Int, bool: Boolean, number: LNumber, string: String) {

  def this(constant: Int) {
    this(2,false,LNumber.makeInteger(constant), null)
  }



  def print(out: Output) {
    `type` match {
      case 0 =>
        out.print("nil")
      case 1 =>
        out.print(if (bool) "true" else "false")
      case 2 =>
        out.print(number.toString)
      case 3 =>
        var newlines: Int = 0
        var unprintable:Int = 0
        for (i <- 0 until string.length()) {
          val c = string.charAt(i)
          if (c == '\n') {
            newlines += 1
          } else if ((c <= 31 && c != '\t') || c >= 127) {
            unprintable += 1
          }
        }
        if(unprintable == 0 && !string.contains("[[") && (newlines > 1 || (newlines == 1 && string.indexOf('\n') != string.length() - 1))) {
          var pipe: Int = 0
          var pipeString: String = "]]"
          while (string.indexOf(pipeString) >= 0) {
            pipe += 1
            pipeString = "]"
            var i: Int = pipe
            while ( {
              i -= 1
              i + 1
            } > 0) pipeString += "="
            pipeString += "]"
          }
          out.print("[")
          while ( {
            pipe -= 1
            pipe + 1
          } > 0) out.print("=")
          out.print("[")
          val indent: Int = out.getIndentationLevel
          out.setIndentationLevel(0)
          out.println()
          out.print(string)
          out.print(pipeString)
          out.setIndentationLevel(indent)
        }
        else {
          out.print("\"")

            var i: Int = 0
            while (i < string.length) {

                val c: Char = string.charAt(i)
                if (c <= 31 || c >= 127) {
                  if (c == 7) {
                    out.print("\\a")
                  }
                  else if (c == 8) {
                    out.print("\\b")
                  }
                  else if (c == 12) {
                    out.print("\\f")
                  }
                  else if (c == 10) {
                    out.print("\\n")
                  }
                  else if (c == 13) {
                    out.print("\\r")
                  }
                  else if (c == 9) {
                    out.print("\\t")
                  }
                  else if (c == 11) {
                    out.print("\\v")
                  }
                  else {
                    val dec: String = Integer.toString(c)
                    var len: Int = dec.length
                    out.print("\\")
                    while ( {
                      len += 1
                      len - 1
                    } < 3) {
                      out.print("0")
                    }
                    out.print(dec)
                  }
                }
                else if (c == 34) {
                  out.print("\\\"")
                }
                else if (c == 92) {
                  out.print("\\\\")
                }
                else {
                  out.print(Character.toString(c))
                }


                i += 1
            }

          out.print("\"")
        }
      case _ =>
        throw new IllegalStateException
    }
  }

  def isNil: Boolean = {
    `type` == 0
  }

  def isBoolean: Boolean = {
    `type` == 1
  }

  def isNumber: Boolean = {
    `type` == 2
  }

  def isInteger: Boolean = {
    number.value == number.value.round
  }

  def asInteger: Int = {
    if (!isInteger) {
      throw new IllegalStateException
    }
    number.value.toInt
  }

  def isString: Boolean = {
    `type` == 3
  }

  def isIdentifier: Boolean = {
    if (!isString) {
      return false
    }
    if (Constant.reservedWords.contains(string)) {
      return false
    }
    if (string.length == 0) {
      return false
    }
    val start: Char = string.charAt(0)
    if (start != '_' && !Character.isLetter(start)) {
      return false
    }
    {
      var i: Int = 1
      while (i < string.length) {

          val next: Char = string.charAt(i)
          if (Character.isLetter(next)) {
          }
          else if (Character.isDigit(next)) {
          }
          else if (next == '_') {
          } else  return false
          i += 1
      }
    }
    true
  }

  def asName: String = {
    if (`type` != 3) {
      throw new IllegalStateException
    }
    string
  }
}