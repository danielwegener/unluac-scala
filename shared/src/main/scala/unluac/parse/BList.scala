package unluac.parse

import scala.collection.mutable

case class BList[T <: BObject](length: BInteger, values: mutable.IndexedSeq[T]) extends BObject {


  def get(index: Int): T = {
    values(index)
  }

  def asArray(array: Array[T]): Array[T] = {
    length.iterate(new Runnable() {
      private var i: Int = 0
      override def run() {
        array(i) = values(i)
        i += 1
      }
    })
    array
  }
}