package unluac.parse

import java.nio.ByteBuffer

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait BObjectType[T <: BObject] {
  def parse(buffer: ByteBuffer, header: BHeader): T

  final def parseList(buffer: ByteBuffer, header: BHeader): BList[T] = {
    val length: BInteger = header.integer.parse(buffer, header)
    val values: mutable.ArrayBuffer[T] = new ArrayBuffer
    length.iterate(new Runnable() {
      def run() {
        values += parse(buffer, header)
      }
    })
    new BList[T](length, values)
  }
}