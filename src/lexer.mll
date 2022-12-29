{

open Lexing
open Parser
exception LexingError of string

let next_line lexbuf =
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <-
    { pos with pos_bol = lexbuf.lex_curr_pos;
               pos_lnum = pos.pos_lnum + 1
    }

}



let white = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule read =
  parse
  | white    { read lexbuf }
  | newline  { next_line lexbuf; read lexbuf }
  | '('      { LEFT_PARENTHESIS }
  | ')'      { RIGHT_PARENTHESIS }
  | "lambda" | '\\' { LAMBDA }
  | "->" { ARROW }
  | id {  IDENTIFIER (Lexing.lexeme lexbuf) }
  | _ { raise (LexingError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }
  | eof      { EOF }