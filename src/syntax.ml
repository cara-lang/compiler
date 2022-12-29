open Base

open Identifier

type unop = 
  | OpNeg
  [@@deriving sexp]

type binop =
  | OpPlus 
  | OpMinus 
  | OpTimes 
  | OpDiv
  [@@deriving sexp]

type expr =
  (* literals *)
  | EInt of int        (* 123 *)
  | EFloat of float    (* -123.45e-7 *)
  | EChar of string    (* 'a' *) (* It's holding a string because of extended grapheme clusters *)
  | EString of string  (* "abc" *)
  | EUnit              (* () *)

  (* collections *)
  | ETuple of expr list  (* (1,"abc"), (1,"abc",2,()) *)
  | EList of expr list   (* [1,2,3], [] *)

  (* calls *)
  | EUnOp of unop * expr           (* -num *)
  | EBinOp of expr * binop * expr  (* 1 + 2 *)
  | ECall of expr * expr list      (* foo(1,2,3) *)

  (* other *)
  | EIdentifier of string list * string  (* IO.println, x, Just, Maybe.Just *)
  | ELambda of string list * expr        (* \(x,y) -> x + y + 1 *)
  | EClosure of string list * expr * expr Map.M(Identifier).t (* lambda along with the environment as of time of definition *)
  [@@deriving sexp]

type bang =
  | BValue of expr             (* foo! *)
  | BCall of expr * expr list  (* foo!(1,2,3) *)
  [@@deriving sexp]

type stmt =
  | SLet of string * expr      (* x = 123 *)
  | SLetBang of string * bang  (* x = foo! *)
  | SBang of bang              (* foo! *)
  (* TODO: function definition *)
  [@@deriving sexp]

type stmt_list = 
  | StmtList of stmt list * expr option
  (* The last expr is the returned one. If None, we return Unit.
     These get chained together:
        x = 123 ----> pure 123 >>= \x  -> ...
        x = foo! ---> foo      >>= \x  -> ...
        foo! -------> foo      >>= \() -> ...
        123 --------> (only allowed in the last position) 123
  *)
  [@@deriving sexp]


let add map k v =
  let added = 
      Map.remove map k
      |> Map.add ~key:k ~data:v
  in 
  match added with
    | `Ok new_map -> new_map
    | `Duplicate -> failwith "Map add - duplicate shouldn't happen"