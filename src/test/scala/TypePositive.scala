package fuselang

import Utils._
import Syntax._

class ParsingPositivea extends org.scalatest.FunSuite {
  test("atoms parseAst") {
    parseAst("1")
    parseAst("true")
    parseAst("true;")
  }

  test("binops") {
    parseAst("1 + 2")
    parseAst("1 + 2;")
    parseAst("1 + 2 * 3;")
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
    parseAst("decl x: bool")
    parseAst("decl x: bit<64>[10 bank 5];")
  }

  test("let") {
    parseAst("let x = 1; x + 2;")
  }

  test("for loop") {
    parseAst("""
      for (let i = 0..10) {
        x + 1;
      }
    """ )

    parseAst("""
      for (let i = 0..10) unroll 5 {
        x + 1;
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

}

class TypeCorrect extends org.scalatest.FunSuite {

  test("Adding sized int to static int") {
    typeCheck("decl x: bit<64>; let y = 1; x + y;")
  }

  test("Reassign bound variable") {
    val e1 = typeCheck("let x = 1; x := 2;")
    assert(e1("x").typ === TStaticInt(2))

    val e2 = typeCheck("decl y: bit<64>; let x = 1; x := y;")
    assert(e2("x").typ === TSizedInt(64))
  }

  test("Static index into array") {
    val e1 = typeCheck("decl a: bit<64>[10]; let x = a[1];")
    assert(e1("x").typ === TSizedInt(64))
    assert(e1("a").conBanks(0) === Set(0))
  }

  test("for loop") {
    val e1 = typeCheck("""
      decl a: bit<64>[10];
      for (let i = 0..10) {
        let x = a[i]
      }
    """ )
    assert(e1("a").conBanks(0) === Set(0))

    val e2 = typeCheck("""
      decl a: bit<64>[10 bank 5];
      for (let i = 0..10) unroll 5 {
        let x = a[i]
      }
    """ )
    assert(e2("a").conBanks(0) === 0.until(5).toSet)
  }

  test("refresh banks") {
    typeCheck("""
      decl a: bit<64>[10 bank 5];
      for (let i = 0..10) unroll 5 {
        let x = a[i];
        ---
        let y = a[i];
      }
    """ )
  }

}
