open Syntax

let rec interpret program =
  match program with
  | EInt i -> i
  | EUnOp (unop,e1) -> (match unop with
      | OpNeg -> -(interpret e1)
    )
  | EBinOp (e1,binop,e2) -> (match binop with
    | OpPlus  -> interpret e1 + interpret e2
    | OpMinus -> interpret e1 - interpret e2
    | OpTimes -> interpret e1 * interpret e2
    | OpDiv   -> interpret e1 / interpret e2
  )

let _ =
  if Array.length Sys.argv != 2 then (
    print_string ("Usage: " ^ Sys.argv.(0) ^ " <SOURCE_FILE>\n");
    exit 0)
  else
    try
      let filename = Sys.argv.(1) in
      let ic = open_in filename in
      let lexbuf = Sedlexing.Utf8.from_channel ic in
      let lexer = Sedlexing.with_tokenizer Lexer.token lexbuf in
      let parser = MenhirLib.Convert.Simplified.traditional2revised Parser.main in
      let program = parser lexer in
      let int = interpret program in
      print_int int;
      print_newline ();
      flush stdout
    with Lexer.Eof -> exit 0
