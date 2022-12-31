{
open Base
open Menhir_parser

exception LexError of string

let[@inline] failwith msg = raise (LexError msg)

let[@inline] illegal c =
  failwith (Printf.sprintf "lexer: unexpected character: '%c'" c)
}

let digit  = ['0'-'9']
let underscore = '_'
let minus = '-'
let exponent = ['e' 'E']
let int   = minus? digit (digit | underscore)*
let float = minus? digit (digit | underscore)* '.' digit (digit | underscore)* (exponent minus? digit+)?
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
  | "//"        { line_comment lexbuf }
  | "#!"        { shebang_comment lexbuf }

  | qualifier as n  { QUALIFIER (n |> String.rstrip ~drop:(fun c -> Char.equal c '.')) }
  | lower_name as n { LOWER_NAME n } (* Note: We must not allow creating EIdentifier ([],"_") or all hell will break lose with holes.  *)
  | upper_name as n { UPPER_NAME n } (*       Perhaps it would be a good idea to namespace the holes with some impossible module name. *)
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
  | '"'  { read_singleline_string (Buffer.create 16) lexbuf }
  | '`'  { read_multiline_string  (Buffer.create 16) lexbuf }
  | '\'' { read_char (Buffer.create 16) lexbuf }

  | '\\' { BACKSLASH }
  | "->" { ARROW }
  | "#(" { LHOLE } (* TODO: would be lovely to have (...) or \(...) syntax instead... perhaps when I have more experience with LR(1) parsers *)

  | '+' { PLUS }
  | '-' { MINUS }
  | '*' { TIMES }
  | '/' { DIV }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ',' { COMMA }
  | '!' { BANG }
  | '=' { EQUALS }
  | '_' { UNDERSCORE }
  | '_' digit+ as n { String.drop_prefix n 1 |> Int.of_string |> HOLE }

  | _ as c { illegal c }

and read_char buf = parse
  | '\''      { if Buffer.length buf > 0
                  then CHAR (Buffer.contents buf) (* to be validated by the parser later to contain exactly one "Extended Grapheme Cluster" *)
                  else failwith "E0019: Empty character"
              }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_char buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_char buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_char buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_char buf lexbuf }
  | newline   { Lexing.new_line lexbuf; failwith "E0017: Unescaped newline in a character"}
  | '\t'      { failwith "E0018: Unescaped tab in a char"}
  | [^ '\'' '\\' '\n' '\r' '\t']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_char buf lexbuf
    }
  | eof { failwith "lexer: unterminated char at EOF" }
  | _   { failwith ("Illegal character: " ^ Lexing.lexeme lexbuf) }

and read_singleline_string buf = parse
  | '"'       { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_singleline_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_singleline_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_singleline_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_singleline_string buf lexbuf }
  | newline   { Lexing.new_line lexbuf; failwith "E0012: Unescaped newline in a single-line string"}
  | '\t'      { failwith "E0014: Unescaped tab in a single-line string"}
  | [^ '"' '\\' '\n' '\r' '\t']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_singleline_string buf lexbuf
    }
  | eof { failwith "lexer: unterminated string at EOF" }
  | _   { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }

and read_multiline_string buf = parse
  | '`'       { STRING (Buffer.contents buf) }
  | '\\' '\\' { Buffer.add_char buf '\\'; read_multiline_string buf lexbuf }
  | '\\' 'n'  { Buffer.add_char buf '\n'; read_multiline_string buf lexbuf }
  | '\\' 'r'  { Buffer.add_char buf '\r'; read_multiline_string buf lexbuf }
  | '\\' 't'  { Buffer.add_char buf '\t'; read_multiline_string buf lexbuf }
  | newline   { Lexing.new_line lexbuf; Buffer.add_char buf '\n'; read_multiline_string buf lexbuf }
  | [^ '`' '\\']+
    { Buffer.add_string buf (Lexing.lexeme lexbuf);
      read_multiline_string buf lexbuf
    }
  | eof { failwith "lexer: unterminated string at EOF" }
  | _   { failwith ("Illegal string character: " ^ Lexing.lexeme lexbuf) }

and line_comment = parse
  | newline { Lexing.new_line lexbuf; EOL }
  | _       { line_comment lexbuf }

(* We'll generate a SHEBANG token; later the parser will check it's the first
one (barring any EOLs), else it will throw an error.  *)
and shebang_comment = parse
  | newline { Lexing.new_line lexbuf; SHEBANG }
  | _       { shebang_comment lexbuf }

and block_comment nesting = parse
  | "/*"    { block_comment (nesting+1) lexbuf }
  | "*/"    { if nesting > 0 then block_comment (nesting - 1) lexbuf }
  | newline { Lexing.new_line lexbuf; block_comment nesting lexbuf }
  | eof     { failwith "E0009: Unfinished block comment" }
  | _       { block_comment nesting lexbuf }