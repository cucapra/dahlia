package fuselang

class TypeCorrect extends org.scalatest.FunSuite {
  import Utils._

  test("atoms compile") {
    compile("1")
    compile("true")
    compile("true;")
  }

  test("binops") {
    compile("1 + 2")
    compile("1 + 2;")
    compile("true == false")
  }

  test("if") {
    compile("if (true) {}")
    compile("if (false) { 1 + 2 }")
    compile("if (false) { 1 + 2 }")
  }

}
