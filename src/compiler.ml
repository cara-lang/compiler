open Core
open Base
open Syntax
open Identifier

let printf = Stdlib.Printf.printf

exception InterpretError of string
let[@inline] interpret_fail msg = raise (InterpretError msg)

(*** INTERPRET **************************************************)

(* They will be named like #1, #2, ... *)
let current_i : int ref =
  ref 0

let next_i () =
  let curr = !current_i in
  current_i := curr + 1;
  curr

let rec interpret env program =
  (*printf("interpreting with env %s\nexpr:\n%s\n\n") (Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier)  Syntax.sexp_of_expr env)) (Sexp.to_string_hum (Syntax.sexp_of_expr program));*)
  match program with
  | EInt _          -> program
  | EFloat _        -> program
  | EChar _         -> program
  | EString _       -> program
  | EUnit           -> program
  | EBool _         -> program
  | EClosure _      -> program
  | ERecordGetter _ -> program
  | EConstructor (name,es) -> 
      EConstructor (name, List.map ~f:(interpret env) es)
  | EIdentifier (q,x) ->
      (match (q,x) with
      | (["IO"],"println") -> program (* kludge *)
      | _ -> 
        (* TODO how to hold all the possible arities and definitions of a given function at once? Where to try them out? In ECall? *)
        (match Map.find env (q,x) with
          | None -> interpret_fail ("interpret: E0001: Unknown variable " ^ identifier_to_string (q,x))
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
          | _ -> interpret_fail "interpret: Can't negate anything that's not an int"
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
        | (e1',e2') -> interpret_fail ("interpret: Unsupported binop for types that aren't two ints\n\n" 
                        ^ (Sexp.to_string_hum (Syntax.sexp_of_expr (EBinOp (e1',binop,e2'))));)
    )
  | ECall (fn,args) -> 
      (match interpret env fn with
        | EClosure (params,body,defenv) -> 
            (*printf("calling closure %s\nwith params %s\n\n")
              (Sexp.to_string_hum (Syntax.sexp_of_expr body))
              (String.concat ~sep:"," params);*)
          (match List.map2 params (List.map ~f:(interpret env) args) ~f:Tuple2.create with
            | Unequal_lengths -> interpret_fail "interpret: Called a function with a wrong number of arguments"
            | Ok pairs ->
               (*printf("Enhancing defenv with %s\n\n") (Sexp.to_string_hum (List.sexp_of_t (Tuple2.sexp_of_t sexp_of_string Syntax.sexp_of_expr) pairs));*)
               let enhanced_env = (List.fold pairs ~init:defenv ~f:(fun env_ (param,arg) -> add env_ ([],param) arg)) in
               (*printf("Enhanced env: %s\n\n") (Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier) Syntax.sexp_of_expr enhanced_env));
               printf("Body before: %s\n\n") (Sexp.to_string_hum (Syntax.sexp_of_expr body));*)
               interpret enhanced_env body

          )
        | ERecordGetter wanted_field ->
          (match args with
            | [arg] -> interpret_getter env wanted_field arg
            | _ -> interpret_fail ("interpret: Trying to call a record getter with " ^ Int.to_string (List.length args) ^ " arguments")
          )
        | e -> interpret_fail 
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
      | _ -> interpret_fail "E0025: If expression with a non-bool condition"
    )

and interpret_getter env wanted_field arg =
  match interpret env arg with
    | ERecord fs -> interpret_record_access env wanted_field fs
    | ETuple  es -> interpret_tuple_access env wanted_field es
    | _ -> interpret_fail ("E0022: Trying to access a record field from a non-record"
            ^ ":\n\n" ^ Sexp.to_string_hum (Syntax.sexp_of_expr arg)
            (*^ "\n\nwith env:\n\n" ^ (Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier) Syntax.sexp_of_expr env))*)
            )

and interpret_record_access env wanted_field fields =
  let found_field = List.find_map fields ~f:(fun (field,expr) -> if String.equal field wanted_field then Some expr else None) in
  match found_field with
    | None -> interpret_fail "E0007: Trying to access a missing record field"
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
              else interpret_fail "E0023: Trying to access a missing tuple field"
         else interpret_fail "E0023: Trying to access a missing tuple field"

and nth_element n env elements =
  match List.nth elements n with
  | None -> interpret_fail "E0023: Trying to access a missing tuple field"
  | Some el -> interpret env el
        


let interpret_bang env = function
  | BValue expr ->
      let _ = interpret env expr in
      interpret_fail "TODO BValue"
  | BCall (fn,args) -> 
      (match (interpret env fn) with
        | EIdentifier (["IO"],"println") -> (* kludge *)
            (match args with
              | [arg] ->
                  printf("%s\n") (expr_to_string env (interpret env arg));
                  EUnit
              | _ -> interpret_fail "Arity error (IO.println!)"
            )
        | _ -> interpret_fail "TODO BCall general case"
      )

let interpret_stmt env stmt = 
  (*printf("interpreting stmt %s\nwith env %s\n\n")
    (Sexp.to_string_hum (Syntax.sexp_of_stmt stmt))
    (Sexp.to_string_hum (Map.sexp_of_m__t (module Identifier) Syntax.sexp_of_expr env));*)
  match stmt with
  | SLet (name,expr) -> 
      let expr' = interpret env expr in
      add env ([],name) expr'
  | SLetBang (name,bang) -> 
      let expr' = interpret_bang env bang in
      add env ([],name) expr'
  | SBang bang -> 
      let _ = interpret_bang env bang in
      env

let interpret_adt_constructor env constructor =
  let expr = 
    if List.is_empty constructor.arguments
    then EConstructor(constructor.name, [])
    else 
      let args = List.map ~f:(fun _ -> "#" ^ Int.to_string (next_i ()))
                          constructor.arguments in
      let ids = List.map ~f:(fun arg -> EIdentifier ([],arg)) args in
      let expr = ELambda(args, EConstructor(constructor.name, ids)) in
      (*printf("Constructor fn: %s\n") (Sexp.to_string_hum (Syntax.sexp_of_expr expr));
      *)
      interpret env expr
  in
  add env ([],constructor.name) expr


let interpret_dtype env typename typevars constructors =
  (* TODO remember the type for the typechecker! *)
  List.fold constructors ~init:env ~f:interpret_adt_constructor


let interpret_decl env decl =
  match decl with
    | DFunction (name,args,body) ->
      let expr = ELambda(args,body) in
      let expr' = interpret env expr in
      (* TODO Is this fine? Where does this break? (Probably with equational style and then with overloading.) *)
      interpret_stmt env (SLet (name, expr'))
    | DTypeAlias _ -> interpret_fail "TODO: interpret_decl: DTypeAlias"
    | DType (typename,typevars,constructors) -> interpret_dtype env typename typevars constructors
    | DStatement stmt -> interpret_stmt env stmt

let rec interpret_decl_list env decls =
  List.fold decls ~init:env ~f:interpret_decl

let interpret_file filename env =
  filename
  |> Parser.parse_file
  |> interpret_decl_list env

let interpret_string string env =
  string
  |> Parser.parse_string
  |> interpret_decl_list env

let rec lex_lexbuf ?(acc=[]) lexbuf =
  let next = Lexer.next_token lexbuf in
  if Token.compare_token next Token.EOF = 0 then
    List.rev (next::acc)
  else
    lex_lexbuf ~acc:(next::acc) lexbuf


let lex_string string =
  let lexbuf = Lexing.from_string string in
  lex_lexbuf lexbuf

let lex_file filename =
  Stdio.In_channel.with_file filename ~f:(fun chan ->
    let lexbuf = Lexing.from_channel chan in
    lex_lexbuf lexbuf
  )

let print_tokens tokens =
  tokens
  |> List.map ~f:Token.show_token
  |> String.concat ~sep:"\n"
  |> eprintf("%s\n")

let print_program program =
  program
  |> sexp_of_list Syntax.sexp_of_decl
  |> Sexp.to_string_hum
  |> eprintf("%s\n")

(*** MAIN *******************************************************)

(*printf("Parsed:\n%s\n") (Sexp.to_string_hum (Syntax.sexp_of_stmt_list stmt_list));*)

let argv = Sys.get_argv ()
let _ =
  if Array.length argv <> 2 then (
    printf ("Usage: %s <SOURCE_FILE>\n") argv.(0);
    Caml.exit 2
  )
  else
      (* Parser.pp_exceptions (); (* enable pretty error messages *)
      *)
      let init_env = Map.empty (module Identifier) in
      (*printf("Interpreted env:\n%s\n") 
        (interpret_file argv.(1) init_env
          |> Map.sexp_of_m__t (module Identifier) Syntax.sexp_of_expr
          |> Sexp.to_string_hum
        );
      *)
      try 
        let _ =
          init_env
          (*|> interpret_string [%blob "stdlib.cara"]*)
          |> interpret_file argv.(1)
        in
        ()
      with
      (* TODO the columns aren't completely correct *)
      | Parser.LexError {msg;loc} ->
          eprintf("\\[%s:%d:%d]\n%s\n")
            loc.loc_start.pos_fname
            loc.loc_start.pos_lnum
            (loc.loc_start.pos_bol + 1)
            msg
      | Parser.ParseError {token;loc} -> 
        (
          eprintf("\\[%s:%d:%d]\nUnexpected token: %s\n\n") 
            loc.loc_start.pos_fname
            loc.loc_start.pos_lnum
            (loc.loc_start.pos_bol + 1)
            (Token.show_token token);
          eprintf("Lexed:\n");
          argv.(1) |> lex_file |> print_tokens;
          eprintf("\n")
        )
      | InterpretError msg ->
        (
          eprintf("Interpreter error: %s\n\n") msg;
          eprintf("Lexed:\n");
          argv.(1) |> lex_file |> print_tokens;
          eprintf("\nParsed:\n");
          argv.(1) |> Parser.parse_file |> print_program;
          eprintf("\n")
        )
      ;
      Stdio.Out_channel.flush Stdio.stdout
