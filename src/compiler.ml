(*
failwith ("TODO SBang " ^ Sexp.to_string_hum (Syntax.sexp_of_bang bang) )

failwith (
            "TODO EIdentifier 2 /// env = "
              ^ Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier) Syntax.sexp_of_expr env) 
              ^ " /// expr = " 
              ^ Sexp.to_string_hum (Syntax.sexp_of_expr program)
          )
*)

open Base
open Syntax

let printf = Stdlib.Printf.printf

let identifier_to_string (q,x) =
  String.concat ~sep:"." q ^ x

module Identifier = struct
  module T = struct
    type t = string list * string [@@deriving sexp_of]
    let compare = [%compare: string list * string]
  end

  include T
  include Comparator.Make (T)
end

let add map k v =
  let added = 
      Map.remove map k
      |> Map.add ~key:k ~data:v
  in 
  match added with
    | `Ok new_map -> new_map
    | `Duplicate -> failwith "Map add - duplicate shouldn't happen"

let rec expr_to_string env = function
  | EInt i    -> Int.to_string i
  | EFloat f  -> Float.to_string f
                  |> String.rstrip ~drop:(fun c -> Char.equal c '.')
  | EString s -> s
  | EIdentifier (q,x) -> (match Map.find env (q,x) with
      | None -> failwith ("unknown identifier " ^ identifier_to_string (q,x))
      | Some e -> expr_to_string env e
    )
  | EUnit         -> "()"
  | ETuple es     -> "(" ^ (String.concat ~sep:"," (List.map es ~f:(expr_to_string env))) ^ ")"
  | EList es      -> "[" ^ (String.concat ~sep:"," (List.map es ~f:(expr_to_string env))) ^ "]"
  | EUnOp _       -> "<unop>"
  | EBinOp _      -> "<binop>"
  | ECall _       -> "<function call>"

let rec interpret env program =
  match program with
  | EInt i -> EInt i
  | EFloat f -> EFloat f
  | EString s -> EString s
  | EUnit -> EUnit
  | EIdentifier (q,x) ->
      (match (q,x) with
      | (["IO"],"println") -> program (* kludge *)
      | _ -> 
        (match Map.find env (q,x) with
          | None -> failwith "TODO EIdentifier - unbound identifier"
          | Some found -> found
        )
      )
  | ETuple es -> ETuple (List.map es ~f:(interpret env))
  | EList es -> EList (List.map es ~f:(interpret env))
  | EUnOp (unop,e1) ->
      (match unop with
        | OpNeg -> (match (interpret env e1) with
          | EInt i -> EInt (neg i)
          | _ -> failwith "Can't negate anything that's not an int"
        )
      )
  | EBinOp (e1,binop,e2) -> (
      match (interpret env e1, interpret env e2) with
        | (EInt i1, EInt i2) -> (
          match binop with
            | OpPlus  -> EInt (i1 + i2)
            | OpMinus -> EInt (i1 - i2)
            | OpTimes -> EInt (i1 * i2)
            | OpDiv   -> EInt (i1 / i2)
          )
        | _ -> failwith "Unsupported binop for types that aren't two ints"
    )
  | ECall (_,_) -> failwith "TODO ECall"

let interpret_bang env = function
  | BValue expr ->
      let _ = interpret env expr in
      failwith "TODO BValue"
  | BCall (fn,args) -> 
      (match (interpret env fn) with
        | EIdentifier (["IO"],"println") ->
            (match args with
              | [arg] ->
                  printf("%s\n") (expr_to_string env (interpret env arg));
                  EUnit
              | _ -> failwith "Arity error (IO.println!)"
            )
        | _ -> failwith "TODO BCall general case"
      )

let interpret_stmt env = function
  | SLet (name,expr) -> 
      let e = interpret env expr in
      add env ([],name) e
  | SLetBang (name,bang) -> 
      let e = interpret_bang env bang in
      add env ([],name) e
  | SBang bang -> 
      let _ = interpret_bang env bang in
      env

let rec interpret_stmt_list env (StmtList (stmts,returned_expr)) =
  match stmts with
    | [] -> Option.map returned_expr ~f:(interpret env)
    | stmt :: rest ->
        let env1 = interpret_stmt env stmt in
        interpret_stmt_list env1 (StmtList (rest, returned_expr))


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
      let _ = 
        Parser.parse_chan ic 
          |> interpret_stmt_list (Map.empty (module Identifier))
      in
      Stdio.Out_channel.flush Stdio.stdout
