package fuselang

import Utils._
import Errors._
import org.scalatest.{FunSpec, FunSuite}

class SimpleTypeNegative extends FunSpec {

  describe("let with explicit type") {
    it("RHS type must be equal to LHS type") {
      assertThrows[UnexpectedType] {
        typeCheck("let x: bit<16> = true")
      }
    }
  }

  describe("Cannot reference undeclared var") {
    it("in top level") {
      assertThrows[UnboundVar] {
        typeCheck("x + 1")
      }
    }
  }

  describe("Shadowing variable names is not allowed") {
    it("in top level") {
      assertThrows[AlreadyBound] {
        typeCheck("let x = 1; let x = 1;")
      }
    }
    it("in if body") {
      assertThrows[AlreadyBound] {
        typeCheck("let x = 1; if(true) { let x = 1; }")
      }
    }
  }

  describe("Variables do not escape their scope") {
    it("in if") {
      assertThrows[UnboundVar] {
        typeCheck("if (true) {let x = 1;}; x + 2;")
      }
    }
    it("in for") {
      assertThrows[UnboundVar] {
        typeCheck("for (let i = 0..10){let x = 1;}; x + 2;")
      }
    }
    it("iterator id in combine block") {
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

  describe("Cannot add int and float") {
    it("constants") {
      assertThrows[BinopError] {
        typeCheck("1 + 2.5")
      }
    }
    it("declaration") {
      assertThrows[BinopError] {
        typeCheck("decl f: float; f + 1")
      }
    }
  }

  describe("Cannot reassign to new type") {
    it("to a register") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("let x = 1; x := 2.5")
      }
    }
  }

  // XXX(rachit): @adrian This seems to be confusing behavior.
  describe("Indexing with static var") {
    it("works without reassigning") {
      typeCheck("decl a: bit<32>[10]; let x = 1; a[x]")
    }
    it("doesnt work with reassigning") {
      assertThrows[InvalidIndex] {
        typeCheck("decl a: bit<32>[10]; let x = 1; x := 2; a[x]")
      }
    }
  }

  describe("Condition in if should be boolean") {
    it("cannot be number") {
      assertThrows[UnexpectedType] {
        typeCheck("if (1) { let x = 10; }")
      }
    }
  }

  describe("Invalid unrolling factor for range") {
    it("unrolling factor not a factor of range length") {
      assertThrows[UnrollRangeError] {
        typeCheck("""
          for (let i = 0..10) unroll 3 {
            let x = 1;
          }
          """ )
      }
    }
  }

  describe("RHS of reduction") {
    it("not an array") {
      assertThrows[ReductionInvalidRHS] {
        typeCheck("let x = 1; x += x;")
      }
    }
    it("not fully banked") {
      assertThrows[ReductionInvalidRHS] {
        typeCheck("decl a: bit<32>[8 bank 2]; let x = 0; x += a;")
      }
    }
    it("not 1 dimensional array") {
      assertThrows[ReductionInvalidRHS] {
        typeCheck("decl a: bit<32>[8 bank 8][10]; let x = 0; x += a;")
      }
    }
  }

  describe("Banking factor not equal to unrolling factor") {
    it("bank not equal") {
      assertThrows[BankUnrollInvalid] {
        typeCheck("""
          decl a: bit<32>[10 bank 5];
          for (let i = 0..10) unroll 2 {
            let x = a[i];
          }
          """ )
      }
    }
    it("bank factor of unroll") {
      assertThrows[BankUnrollInvalid] {
        typeCheck("""
          decl a: bit<32>[8 bank 4];
          for (let i = 0..8) unroll 2 {
            let x = a[i];
          }
          """ )
      }
    }
  }

  describe("Logical sequencing doesnt refresh resources globally") {
    it("when composed with parallel sequence") {
      assertThrows[MsgError] {
        typeCheck("""
          decl a: bit<32>[8];
          {
            a[0];
            ---
            a[0];
          };
          a[0]
          """ )
      }
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
