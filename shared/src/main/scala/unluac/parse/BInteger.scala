package unluac.parse

import java.math.BigInteger

object BInteger {
  private val MAX_INT: BigInteger = BigInteger.valueOf(Integer.MAX_VALUE)
  private val MIN_INT: BigInteger = BigInteger.valueOf(Integer.MIN_VALUE)
}

class BInteger(val big: BigInteger, val n:Int) extends BObject {

  def this(b: BInteger) {
    this(b.big, b.n)
  }

  def this(n: Int) {
    this(null, n)
  }

  def this(big: BigInteger) {
    this(big, 0)
  }

  def asInt: Int = {
    if (big == null) {
      n
    }
    else if (big.compareTo(BInteger.MAX_INT) > 0 || big.compareTo(BInteger.MIN_INT) < 0) {
      throw new IllegalStateException("The size of an integer is outside the range that unluac can handle.")
    }
    else {
      big.intValue
    }
  }

  def iterate(thunk: Runnable) {
    if (big == null) {
      var i: Int = n
      while ( {
        i -= 1
        i + 1
      } != 0) {
        thunk.run()
      }
    }
    else {
      var i: BigInteger = big
      while (i.signum > 0) {
        thunk.run()
        i = i.subtract(BigInteger.ONE)
      }
    }
  }
}