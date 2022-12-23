open Syntax

let rec eval = function
  | ELiteral n -> n
  | EUnOp (unop, e) -> ( match unop with OpNeg -> -eval e)
  | EBinOp (e1, binop, e2) -> (
      match binop with
      | OpPlus -> eval e1 + eval e2
      | OpMinus -> eval e1 - eval e2
      | OpTimes -> eval e1 * eval e2
      | OpDiv -> eval e1 / eval e2)

let _ =
  try
    let lexbuf = Sedlexing.Utf8.from_channel stdin in
    while true do
      let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
      let parser =
        MenhirLib.Convert.Simplified.traditional2revised Parser.main
      in
      let expr = parser lexer in
      let result = eval expr in
      print_int result;
      print_newline ();
      flush stdout
    done
  with Lexer.Eof -> exit 0
