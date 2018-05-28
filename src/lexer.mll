{
open Parser
}

let num = '-'? ['0'-'9'] ['0'-'9']*

let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

let ws = ['\t' ' ' '\n']

rule token =
  parse
  | ws+          { token lexbuf }

  | num as n     { INT (int_of_string n) }

  | ".."         { RANGE_DOTS }
  | ">"          { GT }
  | "<"          { LT }
  | ">="         { GEQ }
  | "<="         { LEQ }

  | "true"       { TRUE }
  | "false"      { FALSE }

  | "let"        { LET }
  | ";"          { SEMICOLON }
  | "="          { EQUAL }

  | id as i      { ID (i) }

  | eof          { EOF }
