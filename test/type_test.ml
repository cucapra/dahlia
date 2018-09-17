(** Use this file to write down end-to-end tests for the compiler toolchain.
    When adding a new file, make sure to add it to [deps] in test/dune. *)
open Test_utils

let%expect_test "let expressions" =
  compile_string "let y = 10;";
  [%expect {| int y = 10; |}]

;;

let%expect_test "add" =
  compile_string "let x = 2.5; let z = 3.5; x + z";
  [%expect {|
    float x = 2.5;
    float z = 3.5;
    x+z |}]
;;

let%expect_test "reassign" =
  compile_string "let x = 2.5; x := 1;";
  [%expect {|
    float x = 2.5;
    x = 1; |}]
;;

let%expect_test "boolean" =
  compile_string "let x = true; let y = false;";
  [%expect {|
    int x = 1;
    int y = 0; |}]
;;

let%expect_test "type aliases get erased" =
  compile_string "type b = bool; type i = int; type x = i;";
  [%expect {| |}]
