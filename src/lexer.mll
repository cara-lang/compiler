{
open Base
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "[lexer] unexpected character: '%c'" c)
}

(* regular expressions *)
let digit  = ['0'-'9']
let underscore = '_'
let minus = '-'
let exponent = ['e' 'E']
let int   = minus? (digit | underscore)+
let float = minus? (digit | underscore)+ '.' (digit | underscore)+ (exponent minus? digit+)?
let newline = "\r\n" | '\r' | '\n'
let whitespace = ' ' | '\t'
let lower = ['a'-'z']
let upper = ['A'-'Z']
let lower_name = lower (lower | upper | digit | underscore)*
let upper_name = upper (lower | upper | digit | underscore)*
let qualifier = upper_name '.'

rule next_token = parse
  | eof         { EOF }
  | whitespace+ { next_token lexbuf }
  | newline     { Lexing.new_line lexbuf; EOL }
  | "/*"        { block_comment 0 lexbuf; next_token lexbuf }
  | "//"        { line_comment lexbuf; next_token lexbuf }
  | "#!"        { line_comment lexbuf; next_token lexbuf }
  (* TODO only allow shebang on the first non-empty line *)

  | qualifier as n  { QUALIFIER (n |> String.rstrip ~drop:(fun c -> Char.equal c '.')) }
  | lower_name as n { LOWER_NAME n }
  | upper_name as n { UPPER_NAME n }
  | float as n      { n
                      |> String.filter ~f:(fun c -> not (Char.equal c '_'))
                      |> Float.of_string
                      |> FLOAT
                    }
  | int as n        { n
                      |> String.filter ~f:(fun c -> not (Char.equal c '_'))
                      |> Int.of_string
                      |> INT
                    }
  | '"'             { read_string (Buffer.create 17) lexbuf }

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '!' { BANG }
  | '=' { EQUALS }

  | _ as c { illegal c }

and read_string buf =
  parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_string buf lexbuf
    }
  | eof { failwith "[lexer] unterminated string at EOF" }
  | _   { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }
  (* TODO "" strings shouldn't be multiline *)

and line_comment = parse
  | newline { Lexing.new_line lexbuf }
  | _       { line_comment lexbuf }

and block_comment nesting = parse
  | "/*"    { block_comment (nesting+1) lexbuf }
  | "*/"    { if nesting > 0 then block_comment (nesting - 1) lexbuf }
  | newline { Lexing.new_line lexbuf; block_comment nesting lexbuf }
  | eof     { failwith "[lexer] unterminated comment at EOF" }
  | _       { block_comment nesting lexbuf }
