package test

import java.io.{IOException, RandomAccessFile}
import java.nio.{ByteBuffer, ByteOrder}
import java.nio.channels.FileChannel

import unluac.parse.{BHeader, LFunction, LLocal, LObject}

object Compare {
  /**
    * Determines if two files of lua bytecode are the same
    * (except possibly for line numbers).
    */
  def bytecode_equal(file1: String, file2: String): Boolean = {
    val main1: LFunction = file_to_function(file1)
    val main2: LFunction = file_to_function(file2)
    function_equal(main1, main2)
  }

  def function_equal(f1: LFunction, f2: LFunction): Boolean = {
    if (f1.maximumStackSize != f2.maximumStackSize) {
      return false
    }
    if (f1.numParams != f2.numParams) {
      return false
    }
    if (f1.numUpValues != f2.numUpValues) {
      return false
    }
    if (f1.vararg != f2.vararg) {
      return false
    }
    if (f1.code.length != f2.code.length) {
      return false
    }
    {
      var i: Int = 0
      while (i < f1.code.length) {
          if (f1.code(i) != f2.code(i)) {
            return false
          }
          i += 1
        }
    }
    if (f1.constants.length != f2.constants.length) {
      return false
    }
    {
      var i: Int = 0
      while (i < f1.constants.length) {
          if (!object_equal(f1.constants(i), f2.constants(i))) {
            return false
          }
          i += 1
      }
    }
    if (f1.locals.length != f2.locals.length) {
      return false
    }
    {
      var i: Int = 0
      while (i < f1.locals.length) {
          if (!local_equal(f1.locals(i), f2.locals(i))) {
            return false
          }
          i += 1
      }
    }
    if (f1.upvalues.length != f2.upvalues.length) {
      return false
    }
    {
      var i: Int = 0
      while (i < f1.upvalues.length) {
          if (!(f1.upvalues(i) == f2.upvalues(i))) {
            return false
          }
          i += 1
      }
    }
    if (f1.functions.length != f2.functions.length) {
      return false
    }
    {
      var i: Int = 0
      while (i < f1.functions.length) {
          if (!function_equal(f1.functions(i), f2.functions(i))) {
            return false
          }
          i += 1
      }
    }
    true
  }

  def object_equal(o1: LObject, o2: LObject): Boolean = {
    o1 == o2
  }

  def local_equal(l1: LLocal, l2: LLocal): Boolean = {
    l1.start == l2.start && l1.end == l2.end && (l1.name == l2.name)
  }

  def file_to_function(filename: String): LFunction = {
    try {
      val file: RandomAccessFile = new RandomAccessFile(filename, "r")
      val buffer: ByteBuffer = ByteBuffer.allocate(file.length.toInt)
      buffer.order(ByteOrder.LITTLE_ENDIAN)
      var len: Int = file.length.toInt
      val in: FileChannel = file.getChannel
      while (len > 0) len -= in.read(buffer)
      buffer.rewind
      val header: BHeader = BHeader.apply(buffer)
      header.function.parse(buffer, header)
    }
    catch {
      case e: IOException => null

    }
  }
}