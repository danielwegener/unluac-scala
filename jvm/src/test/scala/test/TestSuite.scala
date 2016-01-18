package test

import java.io.IOException

import unluac.Main

object TestSuite {
  private val working_dir: String = "./target/"
  private val decompiled: String = "unluac.out"
  private val recompiled: String = "test.out"
}

case class TestSuite(path: String, files: Array[String]) {
  private val ext = ".lua"
  private val extc = ".luac"


  @throws(classOf[IOException])
  private def test(file: String, compiledFile: String): Boolean = {

    Main.decompile(compiledFile, TestSuite.working_dir + TestSuite.decompiled)
    LuaC.compile(TestSuite.working_dir + TestSuite.decompiled, TestSuite.working_dir + TestSuite.recompiled)
    Compare.bytecode_equal(compiledFile, TestSuite.working_dir + TestSuite.recompiled)
  }

  @throws(classOf[IOException])
  def run: Boolean = {
    var passed: Int = 0
    var failed: Int = 0
    for (name <- files) {
      if (test(path + name + ext, path + name + extc)) {
        System.out.println("Passed: " + name)
        passed += 1
      }
      else {
        System.out.println("Failed: " + name)
        failed += 1
      }
    }
    if (failed == 0) {
      System.out.println("All tests passed!")
      true
    }
    else {
      System.out.println("Failed " + failed + " of " + (failed + passed) + " tests.")
      false
    }
  }
}