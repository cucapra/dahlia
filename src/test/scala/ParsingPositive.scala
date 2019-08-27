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
    parseAst("let x: bit<32>; x + 2;")
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

  test("record literals") {
    parseAst("""
      let res: point = { x = 10; y = 10 };
      """ )
  }

  test("array literals") {
    parseAst("""
      let res: bit<32>[10] = { 1, 2, 3 };
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

  test("imports") {
    parseAst("""
      import "print.h" {}
      """ )
    parseAst("""
      import "print.h" {
        def foo(a: bit<32>);
      }
      """ )
  }

  test("simple views") {
    parseAst("""
      view v = a[_ :];
      """ )

    parseAst("""
      view v = a[_ : bank 2];
      """ )

    parseAst("""
      view v = a[4 * i :];
      """ )

    parseAst("""
      view v = a[i + 1! :];
      """ )

    parseAst("""
      view v = a[4 * i : +3];
      """ )

    parseAst("""
      view v = a[i + 1! : +3];
      """ )

    parseAst("""
      view v = a[4 * i : bank 5];
      """ )

    parseAst("""
      view v = a[i + 1! : bank 5];
      """ )

    parseAst("""
      view v = a[4*i:+3 bank 5];
      """ )

    parseAst("""
      view v = a[i + 1! : +3 bank 5];
      """ )
  }

  test("split views") {
    parseAst("""
      split b = a[by 10];
      """ )

    parseAst("""
      split b = a[by 10][by 20];
      """ )
  }

  test("casting") {
    parseAst("""
      let x = (y as bit<32>)
      """ )
    parseAst("""
      let x = (y as float)
      """ )
  }
}
