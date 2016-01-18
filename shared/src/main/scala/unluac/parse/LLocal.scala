package unluac.parse

case class LLocal(name: LString, start: Int, end: Int) extends BObject {

  var forLoop: Boolean = false

  def this(name: LString, start: BInteger, end: BInteger) {
    this(name, start.asInt, end.asInt)
  }

  override def toString: String = {
    name.deref
  }
}