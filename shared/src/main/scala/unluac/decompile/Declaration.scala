package unluac.decompile

import unluac.parse.LLocal

case class Declaration(name: String, begin: Int, end: Int) {
  var register: Int = 0
  /**
    * Whether this is an invisible for-loop book-keeping variable.
    */
  var forLoop: Boolean = false
  /**
    * Whether this is an explicit for-loop declared variable.
    */
  var forLoopExplicit: Boolean = false

  def this(local: LLocal) {
    this(local.toString, local.start,local.end)
  }

}