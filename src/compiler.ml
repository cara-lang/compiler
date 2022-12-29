open Base
open Core
open Syntax
open Identifier

let printf = Stdlib.Printf.printf

let rec expr_to_string env = function
  | EInt i    -> Int.to_string i
  | EFloat f  -> Float.to_string f
                  (* Float.to_string returns 123. instead of 123, so let's strip that *)
                  |> String.rstrip ~drop:(fun c -> Char.equal c '.') 
  | EChar c   -> c
  | EString s -> s
  | EIdentifier (q,x) -> (match Map.find env (q,x) with
      | None -> failwith ("unknown identifier " ^ identifier_to_string (q,x))
      | Some e -> expr_to_string env e
    )
  | EUnit            -> "()"
  | ETuple es        -> "(" ^ (String.concat ~sep:"," (List.map es ~f:(expr_to_string env))) ^ ")"
  | EList es         -> "[" ^ (String.concat ~sep:"," (List.map es ~f:(expr_to_string env))) ^ "]"
  | EUnOp _          -> "<unop>"
  | EBinOp _         -> "<binop>"
  | ECall _          -> "<function call>"
  | ELambda (_,_)    -> "<function>"
  | EClosure (_,_,_) -> "<function>"

let rec interpret env program =
  match program with
  | EInt _ -> program
  | EFloat _ -> program
  | EChar _ -> program
  | EString _ -> program
  | EUnit -> program
  | EClosure _ -> program
  | EIdentifier (q,x) ->
      (match (q,x) with
      | (["IO"],"println") -> program (* kludge *)
      | _ -> 
        (* TODO how to hold all the possible arities and definitions of a given function at once? Where to try them out? In ECall? *)
        (match Map.find env (q,x) with
          | None -> failwith "[interpret]: E0001: Unknown variable"
          | Some found -> found
        )
      )
  | ETuple es -> ETuple (List.map es ~f:(interpret env))
  | EList es -> EList (List.map es ~f:(interpret env))
  | EUnOp (unop,e1) ->
      (match unop with
        | OpNeg -> (match (interpret env e1) with
          | EInt i -> EInt (neg i)
          | _ -> failwith "[interpret]: Can't negate anything that's not an int"
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
        | _ -> failwith "[interpret]: Unsupported binop for types that aren't two ints"
    )
  | ECall (fn,args) -> 
      (match interpret env fn with
        | EClosure (params,body,defenv) -> 
          (match List.map2 params args ~f:Tuple2.create with
            | Unequal_lengths -> failwith "[interpret]: Called a function with a wrong number of arguments"
            | Ok pairs -> interpret (List.fold pairs ~init:defenv ~f:(fun env_ (param,arg) -> add env_ ([],param) arg)) body
          )
        | _ -> failwith "[interpret]: Tried to call a non-closure"
      )
  | ELambda (params,body) -> EClosure (params,body,env) (* magic.gif *)

let interpret_bang env = function
  | BValue expr ->
      let _ = interpret env expr in
      failwith "TODO BValue"
  | BCall (fn,args) -> 
      (match (interpret env fn) with
        | EIdentifier (["IO"],"println") -> (* kludge *)
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
      let expr = Parser.parse_chan ic in
      let env = Map.empty (module Identifier) in
      (* printf("Parsed: %s\n") (Sexp.to_string_hum (Syntax.sexp_of_stmt_list expr)); *)
      let _ = interpret_stmt_list env expr in
      Stdio.Out_channel.flush Stdio.stdout