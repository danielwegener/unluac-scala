package unluac.parse

object LNil {
  val NIL: LNil = new LNil
}

class LNil extends LObject {

  override def equals(o: Any): Boolean = {
    o match {
      case anyRef:AnyRef => this eq anyRef
      case _ => false
    }
  }
}