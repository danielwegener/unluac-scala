package unluac.parse

class LString(val size:BSizeT, _value:String) extends LObject {
  final val value: String = if (_value.length == 0) "" else _value.substring(0, _value.length - 1)


  override def deref: String = {
    value
  }

  override def toString: String = {
    "\"" + value + "\""
  }

  override def equals(o: Any): Boolean = {
    o match {
      case os: LString =>
        os.value == value
      case _ => false
    }
  }
}