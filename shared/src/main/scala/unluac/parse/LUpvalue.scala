package unluac.parse

import java.util.Objects

class LUpvalue extends BObject {
  var instack: Boolean = false
  var idx: Int = 0
  var name: String = null

  override def equals(obj: Any): Boolean = {
    obj match {
      case upvalue: LUpvalue =>
        (instack == upvalue.instack && idx == upvalue.idx) && (Objects.equals(name, upvalue.name) || name != null && (name == upvalue.name))
      case _ =>
        false
    }
  }
}