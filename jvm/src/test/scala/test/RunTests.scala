package test


import org.scalatest.FunSuite

class RunTests extends FunSuite {

  test("run all suites") {
    assert(TestFiles.suite.run === true)
  }
}