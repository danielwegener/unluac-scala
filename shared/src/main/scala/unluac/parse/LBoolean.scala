package unluac.parse

object LBoolean {
  val LTRUE: LBoolean = new LBoolean(true)
  val LFALSE: LBoolean = new LBoolean(false)
}

class LBoolean(val value: Boolean) extends LObject {

  override def toString: String = {
     value.toString
  }

  override def equals(o: Any): Boolean = {
    o match {
      case o:AnyRef => this eq o
      case _ => false
    }
  }
}