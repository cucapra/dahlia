package fuselang

import fuselang.common._
import TestUtils._
import Errors._
import org.scalatest.FunSpec

class TypeCheckerSpec extends FunSpec {

  describe("Let bindings") {

    describe("with explicit type and initializer") {
      it("disallows using smaller sized int in assignment") {
        assertThrows[UnexpectedSubtype] {
          typeCheck("decl a: bit<16>; let x: bit<8> = a;")
        }
      }
      it("RHS type must be equal to LHS type") {
        assertThrows[UnexpectedSubtype] {
          typeCheck("let x: bit<16> = true")
        }
      }
    }

    describe("with explicit type and without initializer") {
      it("works") {
        typeCheck("let x: bit<16>;")
      }
      it("can be assigned to") {
        typeCheck("let x: bit<16>; x := 1;")
      }
      it("requires correct type in assignment") {
        assertThrows[UnexpectedSubtype] {
          typeCheck("let x: bit<16>; x := true;")
        }
      }
    }
  }

  describe("Cannot reference undeclared var") {
    it("in top level") {
      assertThrows[Unbound] {
        typeCheck("x + 1")
      }
    }
  }

  describe("Array access") {
    it("with invalid accessor type") {
      assertThrows[UnexpectedType] {
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
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<64>[10];
          for (let i = 0..10) {
            let x = a[i]
          }
          a[0]
          """ )
      }
    }
    it("consumes all banks with unroll") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<64>[10 bank 5];
          for (let i = 0..10) unroll 5 {
            let x = a[i]
          }
          a[0]
          """ )
      }
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
      assertThrows[Unbound] {
        typeCheck("if (true) {let x = 1;} x + 2;")
      }
    }
    it("in for") {
      assertThrows[Unbound] {
        typeCheck("for (let i = 0..10){let x = 1;} x + 2;")
      }
    }
    it("in while") {
      assertThrows[Unbound] {
        typeCheck("while (true) {let x = 1;} x + 2;")
      }
    }
    it("iterator id in combine block") {
      assertThrows[Unbound] {
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
        }
        for (let i = 0..1) {
          let x = 10;
        }
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
      assertThrows[NoJoin] {
        typeCheck("1 + 2.5")
      }
    }
    it("cannot add float dec and int") {
      assertThrows[NoJoin] {
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
    it("adding static ints does NOT perform type level computation") {
      typeCheck("let x = 1; let y = 2; let z = x + y;")
    }
    it("result of addition upcast to subtype join") {
      typeCheck("decl x: bit<32>; decl y: bit<16>; let z = x + y")
    }
  }

  describe("Reassign") {
    it("cannot reassign to non-subtype") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("let x = 1; x := 2.5")
      }
    }

    it("can reassign decl") {
      typeCheck("decl x: bit<32>; decl y: bit<16>; x := y")
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
          }
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
          }
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
        }
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
      assertThrows[UnexpectedType] {
        typeCheck("let x = 1; x += x;")
      }
    }
    it("RHS should be fully banked") {
      assertThrows[UnexpectedType] {
        typeCheck("decl a: bit<32>[8 bank 2]; let x = 0; x += a;")
      }
    }
    it("RHS should be 1-dimensional array") {
      assertThrows[UnexpectedType] {
        typeCheck("decl a: bit<32>[8 bank 8][10]; let x = 0; x += a;")
      }
    }
    it("outside a loop with fully banked array") {
      typeCheck("""
        decl a: bit<64>[10 bank 10];
        let sum: bit<64> = 0;
        sum += a;
        """ )
    }
    // This is equivalent to the example above
    it ("fully unrolled loop and fully banked array") {
      typeCheck("""
        decl a: bit<64>[10 bank 10];
        let sum: bit<64> = 0;
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
        let sum: bit<64> = 0;
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
        let sum: bit<64> = 0;
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
    it("total resources consumed is union of all resources consumed") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<32>[8];
          decl b: bit<32>[8];
          decl c: bit<32>[8];
          {
            c[0] := 1
            ---
            b[0] := 1;
            ---
            a[0] := 1;
          }
          b[0] := 1
          """ )
      }
    }

    it("doesnt refresh resources globally when composed with parallel composition") {
      assertThrows[AlreadyWrite] {
        typeCheck("""
          decl a: bit<32>[8];
          {
            a[0] := 1;
            ---
            a[0] := 1;
          }
          a[0] := 1
          """ )
      }
    }

    it("allow declarations in first statement to be used in second") {
      typeCheck("""
              let bucket_idx = 10;
              ---
              bucket_idx := 20;
         """ )
    }

    it("check for declarations used in both branches") {
      typeCheck("""
              let test_var = 10;
              {
                test_var := 50;
                ---
                test_var := 30;
              }
         """ )
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

    it("allows same banks to be used with reassignment") {
      typeCheck("""
        decl a: bit<10>[20 bank 10];
        for (let i = 0..20) unroll 10 {
          a[i] := 5;
          ---
          a[i] := 2;
        }
      """ )
    }

    it("reuses banks of multidimensional array") {
      typeCheck("""
        decl a: bit<10>[20 bank 10][10 bank 5];
        for (let i = 0..20) unroll 10 {
          for (let j = 0..10) unroll 5 {
            a[i][j] := 5;
            ---
            a[i][j] := 3;
          }
        }
      """ )
    }
  }

  describe("Capabilities in simple contexts") {
    it("read capabilities end at scope boundaries") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<32>[6 bank 6];

          for(let i = 0..6) { a[0] }
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
            }
          """ )
      }
    }

    it("read capabilities can be used multiple times") {
      typeCheck("""
          decl a: bit<32>[6 bank 6];

          for(let i = 0..6) {
            let x = a[0];
            let y = a[0];
          }
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
      assertThrows[Unbound] {
        typeCheck("""
          def bar(a: bool) { foo(a) }
          def foo(a: bool) { foo(a) }
          """ )
      }
    }

    it("do not allow recursion") {
      assertThrows[Unbound] {
        typeCheck("""
          def bar(a: bool) { bar(a) }
          """ )
      }
    }

    it("allow externs to be defined") {
      typeCheck("""
        def extern bar(a: bool);
        """ )
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
      assertThrows[NoJoin] {
        typeCheck("""
          def bar(a: bit<10>) { a }
          1 + bar(10)
          """ )
      }
    }

    it("Require exact match for array dimensions and banks") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("""
          def foo(a: bit<32>[10 bank 5]) {
          }
          decl b: bit<32>[5 bank 5];
          foo(b)
          """ )
      }
    }
  }

  describe("Function applications w/ externs") {
    it("require the correct types") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("""
          def extern bar(a: bool);
          bar(1)
          """ )
      }
    }

    it("completely consume array parameters") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          def extern bar(a: bit<10>[10 bank 5]);
          decl x: bit<10>[10 bank 5];
          bar(x);
          x[1]
          """ )
      }
    }
  }

  describe("Simple views") {
    it("must have dimensions equal to array") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bit<10>[10 bank 5][10 bank 5];
          view v = a[5 * i :]
          """ )
      }
    }
    it("cannot be nested inside unroll context") {
      assertThrows[ViewInsideUnroll] {
        typeCheck("""
          decl a: bit<10>[16 bank 8];
          for (let i = 0..4) unroll 4 {
            for (let j = 0..4) {
              view v = a[4 * j :]
            }
          }
          """ )
      }
    }
    it("can use loop iterator if not unrolled") {
        typeCheck("""
          decl a: bit<10>[16 bank 8];
          for (let i = 0..4) {
            view v = a[8 * i :]
          }
          """ )
    }
    it("has the same type as the underlying array") {
      assertThrows[NoJoin] {
        typeCheck("""
          decl a: bool[10 bank 5];
          view v = a[0!:];
          v[3] + 1;
          """ )
      }
    }
    it("has the same dimensions as underlying array") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bool[10 bank 5][10 bank 5];
          view v = a[0!:][0!:];
          v[1]
          """ )
      }
    }
    it("cannot be used in the same time step as underlying array") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bool[10 bank 5][10 bank 5];
          view v = a[0!:][0!:];
          a[0][0]; v[0][0];
          """ )
      }
    }
  }

  describe("Split views") {
    it("requires the same dimesions as the underlying array") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bit<32>[10 bank 5][2];
          split v = a[by 5];
          """ )
      }
    }
    it("cannot be created inside an unrolled loop") {
      assertThrows[ViewInsideUnroll] {
        typeCheck("""
          decl a: bit<32>[10 bank 5];
          for (let i = 0 .. 8) unroll 2 {
            split v = a[by 5];
          }
          """ )
      }
    }
    it("requires split factor to divide banking factor") {
      assertThrows[InvalidSplitFactor] {
        typeCheck("""
          decl a: bit<32>[10];
          split v = a[by 5];
          """ )
      }
    }
    it("add another dimension for each non-zero split") {
      assertThrows[IncorrectAccessDims] {
        typeCheck("""
          decl a: bit<32>[10 bank 5];
          split v = a[by 5];
          v[0]
          """ )
      }
    }
    it("can be created inside an normal loop") {
      typeCheck("""
        decl a: bit<32>[10 bank 5];
        split v = a[by 5];
        """ )
    }
  }

  describe("Simple aligned views") {
    it("width must be factor of banking factor") {
      assertThrows[InvalidAlignFactor] {
        typeCheck("""
          decl x: bit<32>;
          decl a: bit<10>[10 bank 5];
          view v = a[3 * x :]
          """ )
      }
    }

    it("width must be a multiple of the banking factor") {
      assertThrows[InvalidAlignFactor] {
        typeCheck("""
          decl x: bit<32>;
          decl a: bit<10>[10 bank 10];
          view v = a[5 * x :]
          """ )
      }
    }

    it("width must be statically known") {
      assertThrows[ParserError] {
        typeCheck("""
          decl x: bit<32>;
          decl a: bit<10>[10 bank 5];
          view v = a[x * x :]
          """ )
      }
    }

    it("suffix factor is a factor of the new banking") {
      typeCheck("""
        decl x: bit<32>;
        decl a: bit<10>[16 bank 8];
        view v = a[6 * x : bank 2]
        """ )
    }
  }

  describe("Simple rotation views") {
    it("can describe arbitrary, unrestricted rotations") {
      typeCheck("""
        decl a: bit<10>[10 bank 5];
        decl i: bit<32>;
        view v = a[i * i ! :]
        """ )
    }
  }

  describe("Gadget checking") {
    it("simple views aliasing same underlying array cannot be used together") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<32>[10 bank 2][10 bank 5];
          view v1 = a[0!:][0!:];
          view v2 = a[1!:][2!:];
          v1[0][0]; v2[0][0];
          """ )
      }
    }
    it("views created from other views cannot be used together") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<32>[10 bank 2][10 bank 5];
          view v1 = a[0!:][0!:];
          view v2 = v1[1!:][2!:];
          a[0][0]; v2[0][0];
          """ )
      }
    }
    it("split views created from the same underlying arrays cannot be used together") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: bit<32>[10 bank 2][10 bank 2];
          view v1 = a[0!:][0!:];
          split v2 = a[by 2][by 2];
          v2[0][1][0][2]; v1[0][0];
          """ )
      }
    }
    it("arrays have fine grained bank tracking") {
        typeCheck("""
          decl a: float[2 bank 2][10 bank 2];
          a[0][0];
          a[1][1];
          """ )
    }
    it("simple views dont have fine grained bank tracking") {
      assertThrows[AlreadyConsumed] {
        typeCheck("""
          decl a: float[10 bank 2][10];
          view v1 = a[2 * 1: +2][0!:];
          v1[0][0]; v1[1][0];
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

    it("can be used for comparisons") {
      typeCheck("""
        let temp = 0;
        for (let i = 0..10) {
          if (i == temp) {
            let x = 0;
          }
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

    it("with arithmetic, can be used for access") {
      typeCheck("""
        decl a: bit<10>[10];
        for (let i = 0..10) {
          a[i * 2];
        }
        """ )
    }
  }

  describe("Records") {
    it("type can be defined") {
      typeCheck("""
        record point {
          x: bit<32>;
          y: bit<32>
        }
        """ )
    }
    it("can be used in a declaration") {
      typeCheck("""
        record point {
          x: bit<32>;
          y: bit<32>
        }
        decl k: point;
        """ )
    }
    it("can be used in a declaration nested declaration") {
      typeCheck("""
        record point {
          x: bit<32>;
          y: bit<32>
        }
        record bars {
          k: point
        }
        """ )
    }
    it("cannot use undefined type aliases") {
      assertThrows[Unbound] {
        typeCheck("""
          record bars {
            k: point
          }
          """ )
      }
    }
    it("cannot contain arrays") {
      assertThrows[ArrayInRecord] {
        typeCheck("""
          record bars {
            k: bit<10>[10]
          }
          """ )
      }
    }
    it("cannot rebind type alias") {
      assertThrows[AlreadyBound] {
        typeCheck("""
          record bars {
            k: bit<32>
          }
          record bars {
            l: bit<32>
          }
          """ )
      }
    }
    it("can access bound field") {
      typeCheck("""
        record point {
          x: bit<32>
        }
        decl k: point;
        let x = k.x;
        """ )
    }
    it("can bound field has the right return type") {
      typeCheck("""
        record point {
          x: bit<32>
        }
        decl k: point;
        let x = k.x + 1;
        """ )
    }
    it("can bound field has the right return type in nested struct") {
      typeCheck("""
        record point {
          x: bit<32>
        }
        record foo {
          p: point
        }
        decl k: foo;
        let x = k.p.x + 1;
        """ )
    }
  }

  describe("Record Literals") {
    it("can be defined with let") {
      typeCheck("""
        record point { x: bit<32>; y: bit<32> }
        let p: point = {x = 1; y = 2 }
        """ )
    }
    it("cannot be defined without explicit type in let") {
      assertThrows[ExplicitTypeMissing] {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> }
          let p = {x = 1; y = 2 }
          """ )
      }
    }
    it("cannot be used inside expressions") {
      assertThrows[NotInBinder] {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> }
          let p = 1 + {x = 1; y = 2 }
          """ )
      }
    }
    it("get the right type") {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> }
          let p: point = {x = 1; y = 2 };
          let f: bit<32> = p.x;
          """ )
    }
    it("cannot have fields missing") {
      assertThrows[MissingField] {
        typeCheck("""
          record point { x: bit<32>; y: bit<32> }
          let p: point = {x = 1};
          """ )
      }
    }
    it("cannot have extra fields") {
      assertThrows[ExtraField] {
        typeCheck("""
          record point { x: bit<32> }
          let p: point = {x = 1; y = 2};
          """ )
      }
    }
  }

  describe("Array literals") {
    it("requires explicit type in the let binder") {
      assertThrows[ExplicitTypeMissing] {
        typeCheck("""
          let x = {1, 2, 3}
          """ )
      }
    }
    it("does not support multidimensional literals") {
      assertThrows[Unsupported] {
        typeCheck("""
          let x: bit<32>[10][10] = {1, 2, 3}
          """ )
      }
    }
    it("requires literal to have the same size") {
      assertThrows[LiteralLengthMismatch] {
        typeCheck("""
          let x: bit<32>[5] = {1, 2, 3}
          """ )
      }
    }
    it("requires subtypes in the array literal") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("""
          let x: bit<32>[3] = {true, false, true}
          """ )
      }
    }
    it("can be banked") {
      typeCheck("""
        let x: bool[3 bank 3] = {true, false, true}
        """ )
    }
    it("can be used without initializer") {
      typeCheck("""
        let x: bool[3] = {true, false, true};
        {
          x[1];
          ---
          x[0] := false
        }
        """ )
    }
  }

  describe("Indexing with dynamic (sized) var") {
    it("works with an unbanked array") {
      typeCheck("decl a: bit<10>[10]; decl x: bit<10>; a[x] := 5")
    }
    it ("doesn't work with banked array") {
      assertThrows[InvalidDynamicIndex] {
        typeCheck("decl a: bit<10>[10 bank 5]; decl x: bit<10>; a[x] := 5")
      }
    }
  }

  describe("Subtyping relations") {
    it("static ints are always subtypes") {
      typeCheck("1 == 2")
    }

    it("smaller sized ints are subtypes of larger sized ints") {
      typeCheck("""
        decl x: bit<16>;
        decl y: bit<32>;
        x == y
        """ )
    }

    it("static ints are subtypes of index types") {
      typeCheck("""
        for (let i = 0..12) {
          i == 1
        }
        """ )
    }

    it("index types are subtypes of sized ints") {
      typeCheck("""
        decl x: bit<32>;
        for (let i = 0..12) {
          i == x
        }
        """ )
    }

    it("index types get upcast to sized int with log2(maxVal)") {
      typeCheck("""
        decl arr:bit<32>[10];
        for (let i = 0..33) {
          arr[5] := i * 1;
        }
        """ )
    }

    it("Array subtyping is not allowed") {
      assertThrows[UnexpectedSubtype] {
        typeCheck("""
          def foo(b: bit<10>[10]) {
            b[0] := 10; // overflows
          }

          decl a: bit<2>[10];
          foo(a)
          """ )
      }
    }

    it("join of index types is dynamic") {
      typeCheck("""
        for (let i = 0..10) {
          for (let j = 0..2) {
              let x = i + j;
            }
        }
        """ )
    }

    it("equal types have joins") {
      typeCheck("""
        let x = true;
        let y = false;
        x == y;

        let i1: bit<32> = 10;
        let i2: bit<32> = 11;
        i1 == i2;
        """ )
    }

    it("float is a subtype of double") {
      typeCheck("""
        decl x: float;
        decl y: float;
        let z: double = x + y;
        """ )
    }

    it("unsigned and signed cannot be compared") {
      assertThrows[NoJoin] {
        typeCheck("""
          decl x: ubit<32>;
          decl y: bit<32>;
          let z = x + y;
          """ )
      }
    }
  }

  describe("Imports") {
    it("adds externs into type checking scope") {
      typeCheck("""
        import "print.cpp" {
          def extern print_vect(f: float[4]);
        }
        decl a: float[4];
        print_vect(a);
        """ )
    }
  }

  describe("Bound Checking") {
    it("static ints smaller than array size are valid") {
      typeCheck("""
        decl a: bit<32>[10];
        let v = a[9];
        """ )
    }

    it("static ints larger than array size fail") {
      assertThrows[IndexOutOfBounds] {
        typeCheck("""
          decl a: bit<32>[10];
          let v = a[10];
          """ )
      }
    }

    it("array access with index types is valid when maxVal <= array length") {
      typeCheck("""
        decl a: bit<32>[10];

        for (let i = 0..10) {
          a[i] := 0;
        }
        """ )
    }

    it("array access with index types fails when maxVal > array length") {
      assertThrows[IndexOutOfBounds] {
        typeCheck("""
          decl a: bit<32>[10];

          for (let i = 0..11) {
            a[i] := 0;
          }
        """ )
      }
    }

    it("simple views with prefixes have their bounds checked") {
      assertThrows[IndexOutOfBounds] {
        typeCheck("""
          decl a: bit<32>[10];
          view v_a = a[0!:+3];
          v_a[3];
          """ )
      }
    }

    it("creation of simple views is bounds checked") {
      assertThrows[IndexOutOfBounds] {
        typeCheck("""
          decl a: bit<32>[10];
          for (let i = 0..10) {
            view v_a = a[i!:+3];
          }
          """ )
      }
    }
  }

  describe("Explicit casting") {
    it("safe to cast integer types to float") {
      typeCheck("""
        decl x: float;
        decl y: bit<32>;
        (y as float) + x;
        """ )
    }
    it("warning when casting float to bit type") {
      typeCheck("""
        decl x: float;
        decl y: bit<32>;
        (x as bit<32>) + y
        """ )
    }
    it("safe to cast integer float to double") {
      typeCheck("""
        decl x: float;
        decl y: double;
        y + (x as double);
        """ )
    }
  }
}
