open Base
open Core
open Syntax
open Identifier

let printf = Stdlib.Printf.printf

(*** INTERPRET **************************************************)

let rec interpret env program =
  (* printf("interpreting with env %s\nexpr:\n%s\n\n") (Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier)  Syntax.sexp_of_expr env)) (Sexp.to_string_hum (Syntax.sexp_of_expr program)); *)
  match program with
  | EInt _          -> program
  | EFloat _        -> program
  | EChar _         -> program
  | EString _       -> program
  | EUnit           -> program
  | EBool _         -> program
  | EClosure _      -> program
  | ERecordGetter _ -> program
  | EIdentifier (q,x) ->
      (match (q,x) with
      | (["IO"],"println") -> program (* kludge *)
      | _ -> 
        (* TODO how to hold all the possible arities and definitions of a given function at once? Where to try them out? In ECall? *)
        (match Map.find env (q,x) with
          | None -> failwith ("interpret: E0001: Unknown variable " ^ identifier_to_string (q,x))
          | Some found -> found
        )
      )
  | ETuple es  -> ETuple  (List.map es ~f:(interpret env))
  | EList es   -> EList   (List.map es ~f:(interpret env))
  | ERecord fs -> ERecord (List.map fs ~f:(fun (f,e) -> (f,interpret env e)))
  | EUnOp (unop,e1) ->
      (match unop with
        | OpNeg -> (match (interpret env e1) with
          | EInt i -> EInt (neg i)
          | _ -> failwith "interpret: Can't negate anything that's not an int"
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
        | (e1',e2') -> failwith ("interpret: Unsupported binop for types that aren't two ints\n\n" 
                        ^ (Sexp.to_string_hum (Syntax.sexp_of_expr (EBinOp (e1',binop,e2'))));)
    )
  | ECall (fn,args) -> 
      (match interpret env fn with
        | EClosure (params,body,defenv) -> 
          (match List.map2 params (List.map ~f:(interpret env) args) ~f:Tuple2.create with
            | Unequal_lengths -> failwith "interpret: Called a function with a wrong number of arguments"
            | Ok pairs ->
               (*printf("Enhancing defenv with %s\n") (Sexp.to_string_hum (List.sexp_of_t (Tuple2.sexp_of_t sexp_of_string Syntax.sexp_of_expr) pairs));*)
               interpret (List.fold pairs ~init:defenv ~f:(fun env_ (param,arg) -> add env_ ([],param) arg)) body
          )
        | ERecordGetter wanted_field ->
          (match args with
            | [arg] -> interpret_getter env wanted_field arg
            | _ -> failwith ("interpret: Trying to call a record getter with " ^ Int.to_string (List.length args) ^ " arguments")
          )
        | e -> failwith 
                ("interpret: Tried to call a non-closure" 
                    ^ "\n\n" ^ (Sexp.to_string_hum (Syntax.sexp_of_expr e))
                    ^ "\n\nwith:\n\n" ^ String.concat ~sep:"," (List.map args ~f:(fun arg -> Sexp.to_string_hum (Syntax.sexp_of_expr arg)))
                )
      )
  | ELambda (params,body) -> EClosure (params,body,env) (* magic.gif *)
  | ERecordGet (e,wanted_field) -> interpret_getter env wanted_field e
  | EIf (c,t,e) -> (match interpret env c with
      | EBool true -> interpret env t
      | EBool false -> interpret env e
      | _ -> failwith "E0025: If expression with a non-bool condition"
    )

and interpret_getter env wanted_field arg =
  match interpret env arg with
    | ERecord fs -> interpret_record_access env wanted_field fs
    | ETuple  es -> interpret_tuple_access env wanted_field es
    | _ -> failwith ("E0022: Trying to access a record field from a non-record"
            ^ ":\n\n" ^ Sexp.to_string_hum (Syntax.sexp_of_expr arg)
            (*^ "\n\nwith env:\n\n" ^ (Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier) Syntax.sexp_of_expr env))*)
            )

and interpret_record_access env wanted_field fields =
  let found_field = List.find_map fields ~f:(fun (field,expr) -> if String.equal field wanted_field then Some expr else None) in
  match found_field with
    | None -> failwith "E0007: Trying to access a missing record field"
    | Some expr -> interpret env expr

and interpret_tuple_access env wanted_field elements =
  match wanted_field with
  | "first"   -> nth_element 0 env elements
  | "second"  -> nth_element 1 env elements
  | "third"   -> nth_element 2 env elements
  | "fourth"  -> nth_element 3 env elements
  | "fifth"   -> nth_element 4 env elements
  | "sixth"   -> nth_element 5 env elements
  | "seventh" -> nth_element 6 env elements
  | "eighth"  -> nth_element 7 env elements
  | "ninth"   -> nth_element 8 env elements
  | "tenth"   -> nth_element 9 env elements
  (* The madness needs to stop _somewhere_ *)
  | _ -> if String.is_prefix wanted_field ~prefix:"el" then
            let without_el = String.drop_prefix wanted_field 2 in
            if String.for_all without_el ~f:Char.is_digit 
              then nth_element (Int.of_string without_el - 1) env elements
              else failwith "E0023: Trying to access a missing tuple field"
         else failwith "E0023: Trying to access a missing tuple field"

and nth_element n env elements =
  match List.nth elements n with
  | None -> failwith "E0023: Trying to access a missing tuple field"
  | Some el -> interpret env el
        


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

let rec interpret_stmt_list env (StmtList (stmts,ret_expr)) =
  match stmts with
    | [] -> (Option.map ret_expr ~f:(interpret env), env)
    | stmt :: rest ->
        let env1 = interpret_stmt env stmt in
        interpret_stmt_list env1 (StmtList (rest, ret_expr))

let interpret_file filename env =
  filename
  |> Parser.parse_file
  |> interpret_stmt_list env
  |> Tuple2.get2

let interpret_string string env =
  string
  |> Parser.parse_string
  |> interpret_stmt_list env
  |> Tuple2.get2

(*** MAIN *******************************************************)

(*printf("Parsed:\n%s\n") (Sexp.to_string_hum (Syntax.sexp_of_stmt_list stmt_list));*)

let argv = Sys.get_argv ()
let _ =
  if Array.length argv <> 2 then (
    printf ("Usage: %s <SOURCE_FILE>\n") argv.(0);
    Caml.exit 2
  )
  else
      Parser.pp_exceptions (); (* enable pretty error messages *)
      let init_env = Map.empty (module Identifier) in
      let _ =
        init_env
        |> interpret_string [%blob "stdlib.cara"]
        |> interpret_file argv.(1)
      in
      Stdio.Out_channel.flush Stdio.stdout
