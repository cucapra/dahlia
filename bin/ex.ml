open Seashell
open Lexer
open Lexing

let _ =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.prog Lexer.token lexbuf in
  failwith "Implement me"
  
