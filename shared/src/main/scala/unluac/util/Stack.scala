package unluac.util

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

class Stack[T] private(private val data:mutable.Buffer[T]) {

  def this() {
    this(new ArrayBuffer[T]())
  }

  def isEmpty: Boolean = data.isEmpty

  def peek: T = data.last

  def pop: T = data.remove(data.size - 1)

  def push(item: T):Unit = data += item

  def size: Int = data.size

  def reverse():Stack[T] = new Stack(data.reverse)
}
