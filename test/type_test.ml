(** Use this file to write down end-to-end tests for the compiler toolchain.
    When adding a new file, make sure to add it to [deps] in test/dune. *)
open Test_utils

let%expect_test "let expressions" =
  compile_string "let y = 10;";
  [%expect {|
    #include "apcint.h"
    int y = 10; |}]

let%expect_test "add" =
  compile_string "let x = 2.5; let z = 3.5; x + z;";
  [%expect {|
    #include "apcint.h"
    float x = 2.5;
    float z = 3.5;
    x+z |}]

let%expect_test "boolean" =
  compile_string "let x = true; let y = false;";
  [%expect {|
    #include "apcint.h"
    int x = 1;
    int y = 0; |}]

let%expect_test "type aliases get erased" =
  compile_string "type b = bool; type i = bit<32>; type x = i;";
  [%expect {| #include "apcint.h" |}]

let%expect_test "Reassign variables" =
  compile_string "let x = true; x := false;";
  [%expect {|
    #include "apcint.h"
    int x = 1;
    x = 0; |}]

let%expect_test "Reassign arrays" =
  compile_string "func foo(a: bit<32>[10], b: bit<32>[10]) { a[0] := b[0]; }";
  [%expect {|
    #include "apcint.h"
    void foo(int32 a[10], int32 b[10]) {
      /* cap read: b[1*(0)] */
      /* cap write: a[1*(0)] */
      a[1*(0)] = b[1*(0)];
    } |}]

let%expect_test "Read from array twice" =
  compile_string "func foo(a: bit<32>[10]) { let x = a[1]; let y = a[1];}";
  [%expect {|
    #include "apcint.h"
    void foo(int32 a[10]) {
      /* cap read: a[1*(1)] */
      int32 x = a[1*(1)];
      int32 y = a[1*(1)];
    } |}]

let%expect_test "Read from array twice with explicit capability" =
  compile_string "func foo(a: bit<32>[10]) { read a[1] as a1; let x = a1; let y = a1;}";
  [%expect {|
    #include "apcint.h"
    void foo(int32 a[10]) {
      /* cap read: a[1*(1)] */
      int32 x = a[1*(1)];
      int32 y = a[1*(1)];
    } |}]

let%expect_test "Write to an array with explicit capability" =
  compile_string "func foo(a: bit<32>[10]) { write a[1] as a1; a1 := 1;}";
  [%expect {|
    #include "apcint.h"
    void foo(int32 a[10]) {
      /* cap write: a[1*(1)] */
      a[1*(1)] = 1;
    } |}]
