package fuselang

import TestUtils._

class ParsingTests extends org.scalatest.FunSuite {
  test("numbers") {
    parseAst("1")
    parseAst("1.25")
    parseAst("0.25")
    parseAst("0x19")
    parseAst("014")
  }

  test("atoms") {
    parseAst("true")
    parseAst("false")
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
    parseAst("true == false")
    parseAst("1 << 2")
    parseAst("1 >> 2")
    parseAst("1 % 2")
    parseAst("true || false")
    parseAst("true && false")
  }

  test("binop precedence order") {
    parseAst("(1 + 2) * 3;")
    parseAst("1 + 2 * 3 >= 10 - 5 / 7;")
    parseAst("1 >> 2 | 3 ^ 4 & 5")
    parseAst("1 >= 2 || 4 < 5")
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

  test("while loop") {
    parseAst("""
      while (false) {
        let x = 1;
        for (let i = 0..10) unroll 5 {
          let y = a[i];
          x + y;
        }
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

  test("functions") {
    parseAst("""
      def foo(a: bit<32>) {}
      """ )

    parseAst("""
      def foo(a: bit<32>[10 bank 5], b: bool) {
        bar(1, 2, 3)
      }
      """ )
  }

  test("external functions") {
    parseAst("""
      def extern foo(a: bit<32>);
      """ )

    parseAst("""
      def extern foo(a: bit<32>[10 bank 5], b: bool);
      """ )
  }

  test("views") {
    parseAst("""
      view v_a = shrink a[4 * i : 4]
      """ )
  }

  test("records definitions") {
    parseAst("""
      record Point {
        x: bit<32>;
        y: bit<32>
      }
      """ )
    parseAst("""
      record Point {
        x: int;
        y: bit<32>
      }
      """ )
  }

  test("records access") {
    parseAst("""
      let k = p.x;
      """ )
    parseAst("""
      let k = foo[i].x;
      """ )
    parseAst("""
      let k = rec.po.x;
      """ )
  }

}
