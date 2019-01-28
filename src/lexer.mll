{
open Lexing
open Parser

exception SyntaxError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }
}

let num = '-'? ['0'-'9'] ['0'-'9']*
let frac = '.' ['0'-'9']+
let fl = '-'? ['0'-'9'] frac?
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*
let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let comment = "//" [^ '\r' '\n']* ['\n' '\r']

rule token =
  parse
  | white        { token lexbuf }
  | comment      { next_line lexbuf; token lexbuf}
  | newline      { next_line lexbuf; token lexbuf}

  | num as n     { INT (int_of_string n) }
  | fl as f      { FLOAT (float_of_string f) }

  | ".."         { RANGE_DOTS }

  | ":="         { REASSIGN }

  | ">"          { GT }
  | "<"          { LT }
  | ">="         { GEQ }
  | "<="         { LEQ }
  | "!="         { NEQ }
  | "=="         { EQ }
  | "+"          { PLUS }
  | "-"          { MINUS }
  | "*"          { TIMES }

  | "&&"         { AND }
  | "||"         { OR }

  | "write"      { WRITE }
  | "read"       { READ }
  | "as"         { AS }

  | "bank"       { BANK }
  | "unroll"     { UNROLL }
  | "func"       { FUNC }
  | "type"       { TYPE }
  | "bit"        { BIT }

  | "bool"       { BOOL_ANNOTATION }
  | "float"      { FLOAT_ANNOTATION }

  | "if"         { IF }
  | "for"        { FOR }
  | "collect"    { COLLECT }
  | "let"        { LET }
  | "true"       { TRUE }
  | "false"      { FALSE }

  | ";"          { SEMICOLON }
  | ":"          { COLON }
  | ","          { COMMA }
  | "="          { EQUAL }

  | "("          { LPAREN }
  | ")"          { RPAREN }
  | "{"          { LBRACK }
  | "}"          { RBRACK }
  | "["          { LSQUARE }
  | "]"          { RSQUARE }

  | id as i      { ID (i) }

  | _            { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof          { EOF }
