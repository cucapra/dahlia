package fuselang

import Utils._
import Errors._
import org.scalatest.FunSuite

class SimpleTypeNegative extends FunSuite {

  test("Cannot reference undeclared var") {
    assertThrows[UnboundVar] {
      typeCheck("x + 1")
    }
  }

  test("shadowing variable names is not allowed") {
    assertThrows[AlreadyBound] {
      typeCheck("let x = 1; let x = 1;")
    }
    assertThrows[AlreadyBound] {
      typeCheck("let x = 1; if(true) { let x = 1; }")
    }
  }

  test("cannot use variables outside their scope") {
    assertThrows[UnboundVar] {
      typeCheck("if (true) {let x = 1;}; x + 2;")
    }
    assertThrows[UnboundVar] {
      typeCheck("for (let i = 0..10){let x = 1;}; x + 2;")
    }
  }

  test("cannot add int and float") {
    assertThrows[BinopError] {
      typeCheck("1 + 2.5")
    }
    assertThrows[BinopError] {
      typeCheck("decl f: float; f + 1")
    }
  }

  test("Cannot reassign to new type") {
    assertThrows[UnexpectedSubtype] {
      typeCheck("let x = 1; x := 2.5")
    }
  }

  // XXX(rachit): @adrian This seems to be confusing behavior.
  test("Indexing after reassigning static var") {
    // This compiles
    typeCheck("decl a: bit<32>[10]; let x = 1; a[x]")
    // This does not
    assertThrows[InvalidIndex] {
      typeCheck("decl a: bit<32>[10]; let x = 1; x := 2; a[x]")
    }
  }

  test("Condition in if should be boolean") {
    assertThrows[UnexpectedType] {
      typeCheck("if (1) { let x = 10; }")
    }
  }

  test("Invalid unrolling factor for range") {
    assertThrows[UnrollRangeError] {
      typeCheck("""
        for (let i = 0..10) unroll 3 {
          let x = 1;
        }
        """ )
    }
  }

  test("RHS of reduction not an array") {
    assertThrows[ReductionInvalidRHS] {
      typeCheck("let x = 1; x += x;")
    }
  }
  test("RHS of reduction not fully banked") {
    assertThrows[ReductionInvalidRHS] {
      typeCheck("decl a: bit<32>[8 bank 2]; let x = 0; x += a;")
    }
  }
  test("RHS of reduction not 1 dimensional array") {
    assertThrows[ReductionInvalidRHS] {
      typeCheck("decl a: bit<32>[8 bank 8][10]; let x = 0; x += a;")
    }
  }

  test("Banking factor not equal to unrolling factor") {
    assertThrows[BankUnrollInvalid] {
      typeCheck("""
        decl a: bit<32>[10 bank 5];
        for (let i = 0..10) unroll 2 {
          let x = a[i];
        }
        """ )
    }
    assertThrows[BankUnrollInvalid] {
      typeCheck("""
        decl a: bit<32>[8 bank 4];
        for (let i = 0..8) unroll 2 {
          let x = a[i];
        }
        """ )
    }
  }

  test("Iterator should not be bound in combine block") {
    assertThrows[UnboundVar] {
      typeCheck("""
        decl a: bit<32>[8];
        for (let i = 0..8) {
        } combine {
          a[i];
        }
        """ )
    }
  }
}

class FileTypeNegative extends FunSuite {

  import java.nio.file.{Files, Paths}
  import collection.JavaConverters._

  val shouldCompile = Paths.get("src/test/resources/should-fail")

  for (file <- Files.newDirectoryStream(shouldCompile).asScala) {
    test(file.toString) {
      val prog = new String(Files.readAllBytes(file))
      assertThrows[RuntimeException] {
        typeCheck(prog)
      }
    }
  }

}
