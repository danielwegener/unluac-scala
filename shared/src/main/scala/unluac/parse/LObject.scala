package unluac.parse

abstract class LObject extends BObject {

  def deref: String = {
    throw new IllegalStateException
  }

  override def equals(o: Any): Boolean
}