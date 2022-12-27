let digit  = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]
let untilEndOfLine = [%sedlex.regexp? Star (Compl '\n'), '\n']
let lineComment    = [%sedlex.regexp? "//", untilEndOfLine]
let shebangComment = [%sedlex.regexp? "#!", untilEndOfLine]

open Parser

exception Eof

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
  | lineComment -> token buf
  | shebangComment -> token buf (* TODO this means it can happen anywhere, but we'd only like it to be allowed on the first line *)
  | '\n' -> EOL
  | number -> INT (int_of_string (Sedlexing.Latin1.lexeme buf))
  | '+' -> PLUS
  | '-' -> MINUS
  | '*' -> TIMES
  | '/' -> DIV
  | '(' -> LPAREN
  | ')' -> RPAREN
  | eof -> raise Eof
  | _ -> failwith "Unexpected character"
