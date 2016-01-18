package unluac.decompile.statement

import java.util

import unluac.decompile.{Declaration, Output}

class Declare(val decls: util.List[Declaration]) extends Statement {

  def print(out: Output) {
    out.print("local ")
    out.print(decls.get(0).name)

      var i: Int = 1
      while (i < decls.size) {
          out.print(", ")
          out.print(decls.get(i).name)
          i += 1
      }

  }
}