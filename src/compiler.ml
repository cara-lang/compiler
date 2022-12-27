open Base
open Syntax

let printf = Stdlib.Printf.printf

let rec interpret program =
  match program with
  | EInt i -> VInt i
  | EString s -> VString s
  | EUnOp (unop,e1) ->(
      match (interpret e1) with
        | VInt i -> (match unop with
            | OpNeg -> VInt (neg i)
        )
        | VString _ -> (match unop with
            | OpNeg -> failwith "Can't negate string"
        )
        | VUnit -> (match unop with
            | OpNeg -> failwith "Can't negate unit"
        )
    )
  | EBinOp (e1,binop,e2) -> (
      match (interpret e1, interpret e2) with
        | (VInt i1, VInt i2) -> (
          match binop with
            | OpPlus  -> VInt (i1 + i2)
            | OpMinus -> VInt (i1 - i2)
            | OpTimes -> VInt (i1 * i2)
            | OpDiv   -> VInt (i1 / i2)
          )
        | _ -> failwith "Unsupported binop"
    )
  | EIoPrintln e -> 
      match (interpret e) with
        | VInt i    -> printf("%d\n") i; VUnit
        | VString s -> printf("%s\n") s; VUnit
        | VUnit     -> printf("()\n");   VUnit

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
      let _ = Parser.parse_chan ic |> interpret in

      Stdio.Out_channel.flush Stdio.stdout
