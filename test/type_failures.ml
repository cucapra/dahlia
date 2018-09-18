(** Use this file to write down end-to-end tests for the compiler toolchain.
    When adding a new file, make sure to add it to [deps] in test/dune. *)
open Test_utils

let%expect_test "cannot add float and int" =
  compile_string_with_failure "let x = 2.5; let z = 3; x + z";
  [%expect {|
    [Type error] can't apply operator '+' to float and idx<3 .. 4, 0 .. 1> |}]
;;

let%expect_test "Cannot access one dimensional array as multidimensional" =
  compile_string_with_failure "func foo(a: int[10]) { a[1][1] }";
  [%expect {|
    [Type Error] array `a' has 1 dimension; attempted array access implies 2 dimensions |}]
;;

let%expect_test "Cannot access non-existent bank" =
  compile_string_with_failure "func foo(a: int[10], x: int) { a{1}[1] }";
  [%expect {|
    [Type error] Bank 1 already consumed for memory `a' |}]
;;

let%expect_test "Cannot assign incorrect type to array" =
  compile_string_with_failure "func foo(a: int[10], x: bool) { a[1] := x; }";
  [%expect {|
    [Type Error] cannot assign value of type `bool' to L-value of type `idx<0 .. 1, -4611686018427387904 .. 4611686018427387903>' |}]
;;

let%expect_test "Cannot reassign to different type" =
  compile_string_with_failure "let x = 2.5; x := 1;";
  [%expect {|
    [Type Error] cannot assign value of type `idx<1 .. 2, 0 .. 1>' to L-value of type `float' |}]
;;

let%expect_test "Cannot use array twice" =
  compile_string_with_failure "func foo(a: int[10]) { let x = a[1]; let y = a[1]; }";
  [%expect {|
    [Type error] Bank 0 already consumed for memory `a' |}]
;;
