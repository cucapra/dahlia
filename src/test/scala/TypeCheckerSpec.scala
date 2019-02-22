package fuselang

import TestUtils._
import Errors._
import Syntax._
import org.scalatest.FunSpec

class TypeCheckerSpec extends FunSpec {

  describe("Let with explicit type") {
    it("assigns explicit type") {
      val e1 = typeCheck("let x: bit<16> = 1;")
      assert(e1("x").typ === TSizedInt(16))
    }

    it("allows using larger sized int in assignment") {
      val e2 = typeCheck("decl a: bit<8>; let x: bit<16> = a;")
      assert(e2("x").typ === TSizedInt(16))
    }

    it("disallows using smaller sized int in assignment") {
      assertThrows[UnexpectedType] {
        typeCheck("decl a: bit<16>; let x: bit<8> = a;")
      }
    }

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

    it("consumes bank without unroll") {
      val e1 = typeCheck("""
        decl a: bit<64>[10];
        for (let i = 0..10) {
          let x = a[i]
        }
        """ )
      assert(e1("a").conBanks(0) === Set(0))
    }

    it("consumes all banks with unroll") {
      val e2 = typeCheck("""
        decl a: bit<64>[10 bank 5];
        for (let i = 0..10) unroll 5 {
          let x = a[i]
        }
        """ )
      assert(e2("a").conBanks(0) === 0.until(5).toSet)
    }

  }

  describe("Variables scoping") {

    describe("shadowing variables not allowed") {
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
    it("in while") {
      assertThrows[UnboundVar] {
        typeCheck("while (true) {let x = 1;}; x + 2;")
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

    it("allows same name in different scopes") {
      typeCheck("""
        for (let i = 0..1) {
          let x = 10;
        };
        for (let i = 0..1) {
          let x = 10;
        };
        """ )
    }
  }

  describe("Binary operations") {

    it("comparisons on floats returns a boolean") {
      typeCheck("if (2.5 < 23.5) { 1 }")
    }

    it("can add sized int to static int") {
      typeCheck("decl x: bit<64>; let y = 1; x + y;")
    }

    it("can add floats") {
      typeCheck("decl f: float; let y = 1.5; f + y")
    }

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
    it("adding static ints performs type level computation") {
      val e1 = typeCheck("let x = 1; let y = 2; let z = x + y;")
      assert(e1("z").typ === TStaticInt(3))
    }
    it("result of addition upcast to subtype join") {
      val e3 = typeCheck("decl x: bit<32>; decl y: bit<16>; let z = x + y")
      assert(e3("z").typ === TSizedInt(32))
    }
  }

  describe("Reassign") {
    it("cannot reassign to non-subtype") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("let x = 1; x := 2.5")
      }
    }

    it("can reassign static type (upcast to SizedInt)") {
      // Assignment to static ints loses information
      val e1 = typeCheck("let x = 1; x := 2;")
      assert(e1("x").typ === TSizedInt(32), "assiging static := static")
    }

    it("can reassign static type to decl (upcast to join)") {
      val e2 = typeCheck("decl y: bit<64>; let x = 1; x := y;")
      assert(e2("x").typ === TSizedInt(64), "assigning static := dynamic")
    }

    it("can reassign decl") {
      val e3 = typeCheck("decl x: bit<32>; decl y: bit<16>; x := y")
      assert(e3("x").typ === TSizedInt(32), "assigning dynamic := dynamic")
      assert(e3("y").typ === TSizedInt(16), "assigning dynamic := dynamic")
    }
  }

  describe("Conditionals (if)") {
    it("condition must be a boolean") {
      assertThrows[UnexpectedType] {
        typeCheck("if (1) { let x = 10; }")
      }
    }
    it("cannot consume same banks in sequenced statements") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<10>[2 bank 2];
          if (true) {
            a[0]
          };
          a[0]
          """ )
      }
    }
    it("cannot consume same banks in sequenced statements from else branch") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<10>[2 bank 2];
          if (true) {
            a[0]
          } else {
            a[1]
          };
          a[1]
          """ )
      }
    }
    it("can consume same banks in branches") {
      typeCheck("""
        decl a: bit<10>[10];
        if (true) {
          a[0]
        } else {
          a[0]
        }
        """ )
    }
    it("can consume bank in sequenced statement not used in either branch") {
      typeCheck("""
        decl a: bit<10>[2 bank 2];
        if (true) {
          a[0]
        } else {
          a[0]
        };
        a[1]
        """ )
    }
    it("can create different capabilities in branches") {
      // See discussion in: https://github.com/cucapra/seashell/pull/81
        typeCheck("""
          decl a: bit<10>[2 bank 2];
          if (true) {
            a[0] := 1;
          } else {
            let x = a[0];
          }
          """ )
    }
  }

  describe("while loops") {
    it("work") {
      typeCheck("""
        while (true) {
          let x = 1;
        }
        """ )
    }
  }

  describe("Ranges") {
    it("Unrolling constant must be factor of length") {
      assertThrows[UnrollRangeError] {
        typeCheck("""
          for (let i = 0..10) unroll 3 {
            let x = 1;
          }
          """ )
      }
    }
  }

  describe("Reductions") {
    it("RHS should be an array") {
      assertThrows[ReductionInvalidRHS] {
        typeCheck("let x = 1; x += x;")
      }
    }
    it("RHS should be fully banked") {
      assertThrows[ReductionInvalidRHS] {
        typeCheck("decl a: bit<32>[8 bank 2]; let x = 0; x += a;")
      }
    }
    it("RHS should be 1-dimensional array") {
      assertThrows[ReductionInvalidRHS] {
        typeCheck("decl a: bit<32>[8 bank 8][10]; let x = 0; x += a;")
      }
    }
    it("outside a loop with fully banked array") {
      typeCheck("""
        decl a: bit<64>[10 bank 10];
        let sum = 0;
        sum += a;
        """ )
    }
    // This is equivalent to the example above
    it ("fully unrolled loop and fully banked array") {
      typeCheck("""
        decl a: bit<64>[10 bank 10];
        let sum = 0;
        for (let i = 0..10) unroll 10 {
          let v = a[i]
          } combine {
            sum += v;
          }
          """ )
    }
  }

  describe("Combine") {
    it("without unrolling") {
      typeCheck("""
        decl a: bit<64>[10];
        let sum = 0;
        for (let i = 0..10) {
          let x = a[i]
          } combine {
            sum += x;
          }
          """ )
    }

    it("with unrolling") {
      typeCheck("""
        decl a: bit<64>[10 bank 5];
        let sum = 0;
        for (let i = 0..10) unroll 5  {
          let x = a[i]
          } combine {
            sum += x;
          }
          """ )
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

  describe("Sequential composition") {
    it("doesnt refresh resources globally when composed with parallel composition") {
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

  describe("Parallel composition") {
    it("allows same banks to used") {
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
    it("read with one constant accessor and one unrolled iterator") {
      typeCheck("""
        decl a: bit<32>[10 bank 5][10 bank 5];
        for (let i = 0..10) {
          for (let j = 0..10) unroll 5 {
            a[0][j];
            ---
            a[j][0];
          }
        }
        """ )
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
    it("read with two dimensional unrolling") {
      typeCheck("""
        decl a: bit<32>[10 bank 5][10 bank 5];
        for (let i = 0..10) unroll 5 {
          for (let j = 0..10) unroll 5 {
            a[j][i];
            ---
            a[i][j];
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

  describe("Loop iterators") {
    it("can be used for arithmetic") {
      typeCheck("""
        for (let i = 0..10) {
          let x = i * 2;
        }
        """ )
    }

    it("can be passed to functions with int types") {
      typeCheck("""
        def test(a: bit<32>) {
          let test2 = a;
        }

        for (let i = 0..5) {
          test(i);
        }
        """ )
    }

    it("can be used with bit shifts") {
      typeCheck("""
        for (let i = 0..10) {
          let x = i | 2;
        }
        """ )
    }

    it("with arithmetic cannot be used for access") {
      assertThrows[InvalidIndex] {
        typeCheck("""
          decl a: bit<10>[10];
          for (let i = 0..10) {
            a[i * 2];
          }
          """ )
      }
    }
  }

  describe("Records") {
    it("type can be defined") {
      typeCheck("""
        record point {
          x: bit<32>;
          y: bit<32>;
        }
        """ )
    }
    it("can be used in a declaration") {
      typeCheck("""
        record point {
          x: bit<32>;
          y: bit<32>;
        };
        decl k: point;
        """ )
    }
    it("can be used in a declaration nested declaration") {
      typeCheck("""
        record point {
          x: bit<32>;
          y: bit<32>;
        };
        record bars {
          k: point;
        }
        """ )
    }
    it("cannot use undefined type aliases") {
      assertThrows[UnboundType] {
        typeCheck("""
          record bars {
            k: point;
          }
          """ )
      }
    }
    it("cannot contain arrays") {
      assertThrows[ArrayInRecord] {
        typeCheck("""
          record bars {
            k: bit<10>[10];
          }
          """ )
      }
    }
    it("cannot rebind type alias") {
      assertThrows[AlreadyBoundType] {
        typeCheck("""
          record bars {
            k: bit<32>;
          }
          record bars {
            l: bit<32>;
          }
          """ )
      }
    }
    it("can access bound field") {
      typeCheck("""
        record point {
          x: bit<32>;
        };
        decl k: point;
        let x = k.x;
        """ )
    }
    it("can bound field has the right return type") {
      typeCheck("""
        record point {
          x: bit<32>;
        };
        decl k: point;
        let x = k.x + 1;
        """ )
    }
    it("can bound field has the right return type in nested struct") {
      typeCheck("""
        record point {
          x: bit<32>;
        };
        record foo {
          p: point;
        }
        decl k: foo;
        let x = k.p.x + 1;
        """ )
    }
  }

  describe("Record Literals") {
    it("can be defined with let") {
      typeCheck("""
        record point { x: bit<32>; y: bit<32> };
        let p: point = {x = 1; y = 2 }
        """ )
    }
    it("cannot be defined without explicit type in let") {
      assertThrows[ExplicitRecTypeMissing] {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> };
          let p = {x = 1; y = 2 }
          """ )
      }
    }
    it("cannot be used inside expressions") {
      assertThrows[RecLiteralNotInBinder] {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> };
          let p = 1 + {x = 1; y = 2 }
          """ )
      }
    }
    it("get the right type") {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> };
          let p: point = {x = 1; y = 2 };
          let f: bit<32> = p.x;
          """ )
    }
    it("cannot have fields missing") {
      assertThrows[MissingField] {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> };
          let p: point = {x = 1};
          """ )
      }
    }
    it("cannot have extra fields") {
      assertThrows[ExtraField] {
        typeCheck("""
          record point { x: bit<32>};
          let p: point = {x = 1; y = 2};
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

}
