package fuselang

import Utils._

class ParsingPositive extends org.scalatest.FunSuite {
  test("atoms parseAst") {
    parseAst("1")
    parseAst("1.25")
    parseAst("0.25")
    parseAst("true")
    parseAst("true;")
  }

  test("comments") {
    parseAst("""
      /* this is a comment
       * on
       * muliple lines
       */
      // this is comment
      x + 1;
      """ )
  }

  test("binops") {
    parseAst("1 + 2")
    parseAst("1 + 2;")
    parseAst("1 + 2.5;")
    parseAst("1 + 2 * 3;")
    parseAst("(1 + 2) * 3;")
    parseAst("1 + 2 * 3 >= 10 - 5 / 7;")
    parseAst("true == false")
  }

  test("if") {
    parseAst("if (true) {}")
    parseAst("if (false) { 1 + 2 }")
    parseAst("if (false) { 1 + 2 }")
  }

  test("decl") {
    parseAst("decl x: bit<64>;")
    parseAst("decl x: bool;")
    parseAst("decl x: bit<64>[10 bank 5];")
  }

  test("let") {
    parseAst("let x = 1; x + 2;")
  }

  test("for loop") {
    parseAst("""
      for (let i = 0..10) unroll 5 {
        x + 1;
      }
    """ )
  }

  test("combiner syntax") {
    parseAst("""
      for (let i = 0..10) {
      } combine {
      }
    """ )

    parseAst("""
      for (let i = 0..10) {
      } combine {
        sum += 10;
        let x = 1;
      }
    """ )
  }

  test("refresh banks") {
    parseAst("""
      x + 1;
      ---
      x + 2;
    """ )
  }

  test("commands") {
    parseAst("""
    {
      x + 1;
    }
      """ )
  }

}

