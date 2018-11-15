(** Use this file to write down end-to-end tests for the compiler toolchain.
    When adding a new file, make sure to add it to [deps] in test/dune. *)
open Test_utils

let%expect_test "cannot add float and int" =
  compile_string_with_failure "let x = 2.5; let z = 3; x + z;";
  [%expect {|
    [Type error] can't apply operator + to float and idx<3..4, 0..1>. |}]

let%expect_test "Cannot access one dimensional array as multidimensional" =
  compile_string_with_failure "func foo(a: bit<32>[10]) { a[1][1]; }";
  [%expect {|
    [Type Error] array `a' has 1 dimension; attempted array access implies 2 dimensions |}]

let%expect_test "Cannot access non-existent bank" =
  compile_string_with_failure "func foo(a: bit<32>[10], x: bit<32>) { a{1}[1]; }";
  [%expect {|
    [Type error] Bank 1 already consumed for memory `a' |}]

let%expect_test "Cannot assign incorrect type to array" =
  compile_string_with_failure "func foo(a: bit<32>[10], x: bool) { a[1] := x; }";
  [%expect {|
    [Type Error] cannot assign value of type `bool' to L-value of type `unsigned bit<32> lin'. |}]

let%expect_test "Cannot reassign to different type" =
  compile_string_with_failure "let x = 2.5; x := 1;";
  [%expect {|
    [Type Error] cannot assign value of type `idx<1..2, 0..1>' to L-value of type `float'. |}]

let%expect_test "Cannot write to array twice" =
  compile_string_with_failure "func foo(a: bit<32>[10]) { a[1] := 1; a[1] := 1; }";
  [%expect {|
    [Type error] Bank 0 already consumed for memory `a5' |}]

let%expect_test "Cannot write to array twice with explicit cap" =
  compile_string_with_failure "func foo(a: bit<32>[10]) { write a[1] as a1; a1 := 1; a1 := 1; }";
  [%expect {|
    [Type error] Bank 0 already consumed for memory `a1' |}]

let%expect_test "Cannot read and write with same array" =
  compile_string_with_failure "func foo(a: bit<32>[10]) { a[1] := 1; let x = a[1]; }";
  [%expect {|
    [Type error] Bank 0 already consumed for memory `a' |}]

let%expect_test "Cannot shadow variables" =
  compile_string_with_failure "func foo(a: bit<32>[10]) { let a = 10; }";
  [%expect {|
    `a' already bound in this context. Cannot shadow variables. |}]

let%expect_test "Cannot reassign int to something stored with more bits" =
  compile_string_with_failure "func foo(a: bit<32>, b: bit<33>) { a := b; }";
  [%expect {|
    [Type Error] Cannot reassign value of type `unsigned bit<32>' to value of type `unsigned bit<33>';
         `unsigned bit<32>' is represented with less bits than `unsigned bit<33>'.|}]

let%expect_test "Non-existent type" =
  compile_string_with_failure "func foo(a: non_type[10]) { let x = 2; }";
  [%expect {| [Type Error] No type definition for `non_type'.; |}]
