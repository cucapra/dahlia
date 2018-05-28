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
  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { TIMES }


  | "for"        { FOR }
  | "let"        { LET }
  | "true"       { TRUE }
  | "false"      { FALSE }

  | ";"          { SEMICOLON }
  | "="          { EQUAL }

  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "{"          { LBRACK }
  | "}"          { RBRACK }

  | id as i      { ID (i) }

  | eof          { EOF }
