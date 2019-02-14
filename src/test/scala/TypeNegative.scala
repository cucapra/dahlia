package fuselang

import TestUtils._
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

  describe("Array access") {
    it("with invalid accessor type") {
      assertThrows[InvalidIndex] {
        typeCheck("""
          decl a: bit<10>[10];
          a[true];
          """ )
      }
    }
    it("with too many dimensions") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bit<10>[10];
          a[1][1];
          """ )
      }
    }
    it("with too few dimensions") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bit<10>[10][10];
          a[1];
          """ )
      }
    }
  }

  // XXX(rachit): This seems like confusing behavior.
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

  describe("Binary operations") {
    it("cannot add int and float") {
      assertThrows[BinopError] {
        typeCheck("1 + 2.5")
      }
    }
    it("cannot add float dec and int") {
      assertThrows[BinopError] {
        typeCheck("decl f: float; f + 1")
      }
    }
    it("comparison not defined for memories") {
      assertThrows[UnexpectedType] {
        typeCheck("decl a: bit<10>[10]; decl b: bit<10>[10]; a == b")
      }
    }
    it("cannot shift floats") {
      assertThrows[BinopError] {
        typeCheck("10.5 << 1")
      }
    }
    it("logical and defined on booleans") {
      assertThrows[BinopError] {
        typeCheck("1 || 2")
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
      assertThrows[AlreadyWrite] {
        typeCheck("""
          decl a: bit<32>[8];
          {
            a[0] := 1;
            ---
            a[0] := 1;
          };
          a[0] := 1
          """ )
      }
    }
  }

  describe("Capabilities in simple contexts") {
    it("read capabilities end at scope boundaries") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<32>[6 bank 6];

          for(let i = 0..6) { a[0] };
          for(let i = 0..6) { a[0] }
          """ )
      }
    }

    it("write capabilities can only be used once") {
      assertThrows[AlreadyWrite] {
        typeCheck("""
            decl a: bit<32>[6 bank 6];

            for(let i = 0..6) {
              a[0] := 1;
              a[0] := 1;
            };
          """ )
      }
    }

    it("read capabilities can be used multiple times") {
      typeCheck("""
          decl a: bit<32>[6 bank 6];

          for(let i = 0..6) {
            let x = a[0];
            let y = a[0];
          };
        """ )
    }

    it("read cannot occur after write") {
      assertThrows[InvalidCap] {
        typeCheck("""
              decl a: bit<32>[6 bank 6];
              for (let i = 0..6) {
                a[0] := 1;
                let x = a[0] + 1;
              }
          """ )
      }
    }

    it("write cannot occur after read") {
      assertThrows[InvalidCap] {
        typeCheck("""
              decl a: bit<32>[6 bank 6];
              for (let i = 0..6) {
                let x = a[0] + 1;
                a[0] := 1;
              }
          """ )
      }
    }

    it("read after write in same context with seq composition") {
        typeCheck("""
              decl a: bit<32>[6 bank 6];
              for (let i = 0..6) {
                let x = a[0] + 1;
                ---
                a[0] := 1;
              }
          """ )
    }

    it("write after read in same context with seq composition") {
        typeCheck("""
              decl a: bit<32>[6 bank 6];
              for (let i = 0..6) {
                a[0] := 1;
                ---
                let x = a[0] + 1;
              }
          """ )
    }

    it("write after write in same context with seq composition") {
        typeCheck("""
              decl a: bit<32>[6 bank 6];
              for (let i = 0..6) {
                a[0] := 1;
                ---
                a[0] := 2;
              }
          """ )
    }

  }

  describe("Capabilities in unrolled context") {
    it("write in one unrolled loop and a constant access") {
      assertThrows[InsufficientResourcesInUnrollContext] {
        typeCheck("""
          decl a: bit<32>[10];
          for (let i = 0..10) unroll 5 {
            a[0] := 1
          }
          """ )
      }
    }
    it("write in two unrolled loops and incorrect idx accessor") {
      assertThrows[InsufficientResourcesInUnrollContext] {
        typeCheck("""
          decl a: bit<32>[10][10 bank 5];
          for (let i = 0..10) {
            for (let j = 0..10) unroll 5 {
              a[i][0] := 1
            }
          }
          """ )
      }
    }
    it("write with three loops, 2 unrolled") {
      assertThrows[InsufficientResourcesInUnrollContext] {
        typeCheck("""
          decl a: bit<32>[10][10 bank 5];
          for (let k = 0..10) {
            for (let i = 0..9) unroll 3 {
              for (let j = 0..10) unroll 5 {
                a[k][j] := 1
              }
            }
          }
          """ )
      }
    }

    it("read in one unrolled loop and a constant access") {
      typeCheck("""
        decl a: bit<32>[10];
        for (let i = 0..10) unroll 5 {
          let x = a[0]
        }
        """ )
    }
    it("read in two unrolled loops and incorrect idx accessor") {
      typeCheck("""
        decl a: bit<32>[10][10 bank 5];
        for (let i = 0..10) {
          for (let j = 0..10) unroll 5 {
            let x = a[i][0];
          }
        }
        """ )
    }
    it("read with three loops, 2 unrolled") {
      typeCheck("""
        decl a: bit<32>[10][10 bank 5];
        for (let k = 0..10) {
          for (let i = 0..9) unroll 3 {
            for (let j = 0..10) unroll 5 {
              let x = a[k][j]
            }
          }
        }
        """ )
    }

  }

  describe("Functions") {
    it("cannot have same name for multiple params") {
      assertThrows[AlreadyBound] {
        typeCheck("""
          def foo(a: bool, a: bit<10>) {}
          """ )
      }
    }

    it("cannot be used before defintion") {
      assertThrows[UnboundVar] {
        typeCheck("""
          def bar(a: bool) { foo(a) }
          def foo(a: bool) { foo(a) }
          """ )
      }
    }

    it("do no allow recursion") {
      assertThrows[UnboundVar] {
        typeCheck("""
          def bar(a: bool) { bar(a) }
          """ )
      }
    }
  }

  describe("Function applications") {
    it("require the correct types") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("""
          def bar(a: bool) { }
          bar(1)
          """ )
      }
    }

    it("completely consume array parameters") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          def bar(a: bit<10>[10 bank 5]) { }
          decl x: bit<10>[10 bank 5];
          bar(x);
          x[1]
          """ )
      }
    }

    it("do not return values") {
      assertThrows[BinopError] {
        typeCheck("""
          def bar(a: bit<10>) { a }
          1 + bar(10)
          """ )
      }
    }
  }

  describe("Shrink views") {
    it("width must be equal to step") {
      assertThrows[MalformedShrink] {
        typeCheck("""
          decl a: bit<10>[10];
          view v = shrink a[3 * i : 1]
          """ )
      }
    }
    it("width must be factor of banking factor") {
      assertThrows[InvalidShrinkWidth] {
        typeCheck("""
          decl a: bit<10>[10 bank 5];
          view v = shrink a[3 * i : 3]
          """ )
      }
    }
    it("must have dimensions equal to array") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bit<10>[10 bank 5][10 bank 5];
          view v = shrink a[5 * i : 5]
          """ )
      }
    }
    it("cannot be inside unrolled context") {
      assertThrows[ViewInsideUnroll] {
        typeCheck("""
          decl a: bit<10>[16 bank 8];
          for (let i = 0..4) unroll 4 {
            view v = shrink a[4 * i : 4]
          }
          """ )
      }
    }
    it("cannot be nested inside unroll context") {
      assertThrows[ViewInsideUnroll] {
        typeCheck("""
          decl a: bit<10>[16 bank 8];
          for (let i = 0..4) unroll 4 {
            for (let j = 0..4) {
              view v = shrink a[4 * j : 4]
            }
          }
          """ )
      }
    }
    it("has the same type as the underlying array") {
      assertThrows[BinopError] {
        typeCheck("""
          decl a: bool[10 bank 5];
          view v = shrink a[0 : 5];
          v[3] + 1;
          """ )
      }
    }
    it("has the same dimensions as underlying array") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bool[10 bank 5][10 bank 5];
          view v = shrink a[0 : 5][0 : 5];
          v[1]
          """ )
      }
    }
    it("completely consumes underlying array from context") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bool[10 bank 5][10 bank 5];
          view v = shrink a[0 : 5][0 : 5];
          a[0][0]
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
      assertThrows[TypeError] {
        Compiler.compileStringWithError(prog)
      }
    }
  }

}
