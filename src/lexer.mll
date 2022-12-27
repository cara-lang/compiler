{
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let digit  = ['0'-'9']
let number = digit+
let newline = "\r\n" | '\r' | '\n'
let whitespace = ' ' | '\t'

rule next_token = parse
  | eof         { EOF }
  | whitespace+ { next_token lexbuf }
  | newline     { Lexing.new_line lexbuf; EOL }
  | "/*"        { block_comment 0 lexbuf; next_token lexbuf }
  | "//"        { line_comment lexbuf }
  | "#!"        { line_comment lexbuf }
  (* TODO only allow shebang on the first non-empty line *)

  | number as n { INT (int_of_string n) }

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }

  | _ as c { illegal c }

and line_comment = parse
  | newline { Lexing.new_line lexbuf; EOL }
  | _       { line_comment lexbuf }

and block_comment nesting = parse
  | "/*"    { block_comment (nesting+1) lexbuf }
  | "*/"    { if nesting > 0 then block_comment (nesting - 1) lexbuf }
  | newline { Lexing.new_line lexbuf; block_comment nesting lexbuf }
  | eof     { failwith "[lexer] unterminated comment at EOF" }
  | _       { block_comment nesting lexbuf }

