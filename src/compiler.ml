open Base
open Syntax

let printf = Stdlib.Printf.printf

let rec eval program =
  match program with
  | EInt i -> i
  | EUnOp (unop,e1) -> (match unop with
      | OpNeg -> -(eval e1)
    )
  | EBinOp (e1,binop,e2) -> (match binop with
    | OpPlus  -> eval e1 + eval e2
    | OpMinus -> eval e1 - eval e2
    | OpTimes -> eval e1 * eval e2
    | OpDiv   -> eval e1 / eval e2
  )

let argv = Sys.get_argv ()
let _ =
  if Array.length argv <> 2 then (
    Stdio.Out_channel.output_string Stdio.stdout
      ("Usage: " ^ argv.(0) ^ " <SOURCE_FILE>\n");
    Caml.exit 0)
  else
      Parser.pp_exceptions (); (* enable pretty error messages *)

      let filename = argv.(1) in
      let ic = Stdio.In_channel.create filename in

      Parser.parse_chan ic
      |> eval
      |> printf("%d\n");

      Stdio.Out_channel.flush Stdio.stdout
