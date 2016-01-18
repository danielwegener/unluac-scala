package unluac.parse

object LNumber {
  def makeInteger(number: Int): LNumber = {
    new LIntNumber(number)
  }
}

abstract class LNumber extends LObject {
  override def toString: String

  def value: Double
}

case class LFloatNumber(number: Float) extends LNumber {

  override def toString: String = {
    if (number == number.round.toFloat) {
      Integer.toString(number.toInt)
    }
    else {
      number.toString
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case number1: LFloatNumber =>
        return number == number1.number
      case number1: LNumber =>
        return value == number1.value
      case _ =>
    }
    false
  }

  def value: Double = {
    number
  }
}

case class LDoubleNumber(number: Double) extends LNumber {

  override def toString: String = {
    if (number == number.round.toDouble) {
      number.toLong.toString
    }
    else {
      number.toString
    }
  }

  override def equals(o: Any): Boolean = {
    o match {
      case number1: LDoubleNumber =>
        number == number1.number
      case number1: LNumber =>
        value == number1.value
      case _ => false
    }
  }

  def value: Double = {
    number
  }
}

case class LIntNumber(number: Int) extends LNumber {

  override def toString: String = {
    Integer.toString(number)
  }

  override def equals(o: Any): Boolean = {
    o match {
      case number1: LIntNumber =>
        number == number1.number
      case number1: LNumber =>
        value == number1.value
      case _ => false
    }
  }

  def value: Double = number
}

case class LLongNumber(number: Long) extends LNumber {

  override def toString: String = {
    number.toString
  }

  override def equals(o: Any): Boolean = {
    o match {
      case number1: LLongNumber =>
        number == number1.number
      case number1: LNumber =>
        value == number1.value
      case _ => false
    }
  }

  def value: Double = { number }
}