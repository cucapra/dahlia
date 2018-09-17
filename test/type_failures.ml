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
    [Type error] memory `a' illegal access: bank 1 |}]
;;
