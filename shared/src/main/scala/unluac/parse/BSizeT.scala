package unluac.parse

import java.math.BigInteger

class BSizeT(big:BigInteger,n:Int) extends BInteger(big,n) {

  def this(b: BInteger) {
    this(b.big, b.n)
  }

  def this(n: Int) {
    this(null, n)
  }

  def this(n: BigInteger) {
    this(n,0)
  }
}