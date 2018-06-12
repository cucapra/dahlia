open Seashell
open OUnit

let cmd_of_file filepath =
  "test/" ^ filepath
  |> open_in
  |> Lexing.from_channel
  |> Parser.prog Lexer.token

let result id prog =
  cmd_of_file prog
  |> fun c -> Eval.eval_command (c, Eval.empty_env)
  |> Eval.stringified_binding id

let boolops0 = [

  "bools0_tat" >:: (fun _ -> assert_equal "true" (result "t_and_t" "bools0.ss"));
  "bools0_taf" >:: (fun _ -> assert_equal "false" (result "t_and_f" "bools0.ss"));
  "bools0_fat" >:: (fun _ -> assert_equal "false" (result "f_and_t" "bools0.ss"));
  "bools0_faf" >:: (fun _ -> assert_equal "false" (result "f_and_f" "bools0.ss"));

  "bools0_tot" >:: (fun _ -> assert_equal "true" (result "t_or_t" "bools0.ss"));
  "bools0_tof" >:: (fun _ -> assert_equal "true" (result "t_or_f" "bools0.ss"));
  "bools0_fot" >:: (fun _ -> assert_equal "true" (result "f_or_t" "bools0.ss"));
  "bools0_fof" >:: (fun _ -> assert_equal "false" (result "f_or_f" "bools0.ss"));

]

let assign0 = [

  "assign0_x" >:: (fun _ -> assert_equal "5" (result "x" "assignment.ss"));
  "assign0_y" >:: (fun _ -> assert_equal "2" (result "y" "assignment.ss"));
  "assign0_z" >:: (fun _ -> assert_equal "2" (result "z" "assignment.ss"));

]

let array_access = [
  
  "access_in_cond" >:: (fun _ -> assert_equal "2" (result "x" "array_access1.ss"));

]

let suite = "Intepreter test suite" >:::
  boolops0 @ assign0 @ array_access

let _ = run_test_tt_main suite
