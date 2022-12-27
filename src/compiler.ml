open Base
open Syntax

let printf = Stdlib.Printf.printf

let rec expr_to_string = function
  | EInt i     -> Int.to_string i
  | EString s  -> s
  | EUnit      -> "()"
  | ETuple es  -> "(" ^ (String.concat ~sep:"," (List.map es ~f:expr_to_string)) ^ ")"
  | EIoPrintln -> "<IO.println>"
  | EUnOp _    -> "<unop>"
  | EBinOp _   -> "<binop>"
  | ECall _    -> "<function call>"

let rec interpret program =
  match program with
  | EInt i -> EInt i
  | EString s -> EString s
  | EUnit -> EUnit
  | ETuple es -> ETuple (List.map es ~f:interpret)
  | EIoPrintln -> EIoPrintln
  | EUnOp (unop,e1) ->
      (match unop with
        | OpNeg -> (match (interpret e1) with
          | EInt i -> EInt (neg i)
          | _ -> failwith "Can't negate anything that's not an int"
        )
      )
  | EBinOp (e1,binop,e2) -> (
      match (interpret e1, interpret e2) with
        | (EInt i1, EInt i2) -> (
          match binop with
            | OpPlus  -> EInt (i1 + i2)
            | OpMinus -> EInt (i1 - i2)
            | OpTimes -> EInt (i1 * i2)
            | OpDiv   -> EInt (i1 / i2)
          )
        | _ -> failwith "Unsupported binop for types that aren't two ints"
    )
  | ECall (fn,args) ->
      (match fn with
        | EIoPrintln -> 
            (match args with
              | [arg] ->
                  printf("%s\n") (expr_to_string (interpret arg));
                  EUnit
              | _ -> failwith "Arity error (IO.println)"
            )
        | _ -> failwith "Can't call anything that's not IO.println"
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
      let _ = Parser.parse_chan ic |> interpret in

      Stdio.Out_channel.flush Stdio.stdout
