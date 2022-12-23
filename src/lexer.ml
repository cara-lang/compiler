let digit = [%sedlex.regexp? '0' .. '9']
let number = [%sedlex.regexp? Plus digit]

open Parser

exception Eof

let rec token buf =
  match%sedlex buf with
  | Plus (Chars " \t") -> token buf
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
