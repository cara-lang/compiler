open Base
open Core

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
  | ETuple of expr list             (* (1,"abc"), (1,"abc",2,()) *)
  | EList of expr list              (* [1,2,3], [] *)
  | ERecord of (string * expr) list (* {}, {a:123,b:True} *)

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
  (* The last expr is the returned one. If None, we return Unit.  These get
     chained together: either in the imperative way (for the interpreter and the
     IO monad -- I suspect this will stop being enough pretty soon), or more
     generally via monadic bind:

        x = 123 ----> pure 123 >>= \x  -> ...
        x = foo! ---> foo      >>= \x  -> ...
        foo! -------> foo      >>= \() -> ...
        123 --------> (only allowed in the last position, this is an expr, not a stmt) 123
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

type holes =
  | NoHoles
  | OnlyNumbered of int (* max *)
  | OnlyUnderscore
  | Mixed

let combine_holes h1 h2 = 
  match (h1, h2) with
    | (NoHoles, _) -> h2
    | (_, NoHoles) -> h1
    | (Mixed, _) -> h1
    | (_, Mixed) -> h2
    | (OnlyNumbered m1, OnlyNumbered m2) -> OnlyNumbered (max m1 m2)
    | (OnlyNumbered _, OnlyUnderscore) -> Mixed
    | (OnlyUnderscore, OnlyNumbered _) -> Mixed
    | (OnlyUnderscore, OnlyUnderscore) -> OnlyUnderscore

let rec analyze_holes = function
  | EIdentifier ([],x) -> if Char.equal '_' (String.get x 0)
                          then if String.length x = 1
                               then OnlyUnderscore (* _ *)
                               else OnlyNumbered (Int.of_string (String.drop_prefix x 1)) (* _3 *)
                          else NoHoles
  | EIdentifier _ -> NoHoles
  | EInt _    -> NoHoles
  | EFloat _  -> NoHoles
  | EChar _   -> NoHoles
  | EString _ -> NoHoles
  | EUnit     -> NoHoles
  | ETuple es  -> analyze_list es
  | EList es   -> analyze_list es
  | ERecord fs -> analyze_list (List.map ~f:Tuple2.get2 fs)
  | ELambda (_,e)    -> analyze_holes e
  | EClosure (_,e,_) -> analyze_holes e
  | EUnOp (_,e)      -> analyze_holes e
  | EBinOp (e1,_,e2) -> combine_holes (analyze_holes e1) (analyze_holes e2)
  | ECall (fn,args)  -> combine_holes (analyze_holes fn) (analyze_list args)

and analyze_list es = es |> List.map ~f:analyze_holes |> List.fold ~init:NoHoles ~f:combine_holes

let lambda_with_holes body =
  match analyze_holes body with
    | NoHoles -> ELambda ([], body)
    | OnlyUnderscore -> ELambda (["_"], body)
    | OnlyNumbered max_hole -> 
        let params = List.range 1 (max_hole + 1) |> List.map ~f:(fun i -> "_" ^ Int.to_string i) in
        ELambda (params, body)
    | Mixed -> failwith "E0020: Anonymous function shorthand with mixed holes"

let record fs =
  if List.contains_dup fs ~compare:(fun (f1,_) (f2,_) -> String.compare f1 f2)
  then failwith "E0006: Record created with duplicate fields"
  else ERecord fs