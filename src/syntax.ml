open Base
open Core
open Identifier

type unop = 
  | OpNegateNum  (* - *)
  | OpNegateBool (* ! *)
  | OpNegateBin  (* ~ *)
  [@@deriving sexp]

type binop =
  (* numeric *)
  | OpPlus  (* +  *)
  | OpMinus (* -  *)
  | OpTimes (* *  *)
  | OpDiv   (* /  *)
  | OpMod   (* %  *)
  | OpPow   (* ** *)
  (* binary *)
  | OpOrBin   (* |   *)
  | OpAndBin  (* &   *)
  | OpXorBin  (* ^   *)
  | OpShiftL  (* <<  *)
  | OpShiftR  (* >>  *)
  | OpShiftRU (* >>> *)
  (* comparisons and booleans *)
  | OpLte     (* <= *)
  | OpLt      (* <  *)
  | OpEq      (* == *)
  | OpNeq     (* != *)
  | OpGt      (* >  *)
  | OpGte     (* >= *)
  | OpOrBool  (* || *)
  | OpAndBool (* && *)
  (* appendables *)
  | OpAppend (* ++ *)
  (* ranges *)
  | OpRangeInclusive (* ..  *)
  | OpRangeExclusive (* ... *)
  [@@deriving sexp, show]

type arg_list = string list (* TODO patterns? *)
  [@@deriving sexp]

type expr =
  (* literals *)
  | EInt of int        (* 123 *)
  | EFloat of float    (* -123.45e-7 *)
  | EChar of string    (* 'a' *) (* It's holding a string because of extended grapheme clusters *)
  | EString of string  (* "abc" *)
  | EUnit              (* () *)
  | EBool of bool      (* True, False *)

  (* collections *)
  | ETuple of expr list                (* (1,"abc"), (1,"abc",2,()) *)
  | EList of expr list                 (* [1,2,3], [] *)
  | ERecord of (string * expr) list    (* {}, {a:123,b:True} *)
  | EConstructor of string * expr list (* Just(1) *)

  (* calls *)
  | EUnOp of unop * expr           (* -num *)
  | EBinOp of expr * binop * expr  (* 1 + 2 *)
  | ECall of expr * expr list      (* foo(1,2,3) *)
  | ERecordGet of expr * string    (* r.a *)
  | EPipeline of expr * expr       (* a |> b *)

  (* other *)
  | EIdentifier of Identifier.t    (* IO.println, x, Just, Maybe.Just *)
  | ELambda of arg_list * expr  (* \(x,y) -> x + y + 1 *)
  | EClosure of arg_list * expr * expr Map.M(Identifier).t (* lambda along with the environment as of time of definition *)
  | ERecordGetter of string  (* .a, .el0 *)
  | EIf of expr * expr * expr  (* if True then 1 else 2 *)
  [@@deriving sexp]

type bang =
  | BValue of expr             (* foo! *)
  | BCall of expr * expr list  (* foo!(1,2,3) *)
  [@@deriving sexp]

type let_modifier =
  | LNoModifier
  | LPrivate
  [@@deriving sexp]

type stmt =
  | SLet of let_modifier * string * expr (* x = 123 *)
  | SLetBang of string * bang            (* x = foo! *)
  | SBang of bang                        (* foo! *)
  [@@deriving sexp]

(* The last expr is the returned one. If None, we return Unit. These get
    chained together: either in the imperative way (for the interpreter and the
    IO monad -- I suspect this will stop being enough pretty soon), or more
    generally via monadic bind:

      x = 123 ----> pure 123 >>= \x  -> ...
      x = foo! ---> foo      >>= \x  -> ...
      foo! -------> foo      >>= \() -> ...
      123 --------> (only allowed in the last position, this is an expr, not a stmt) 123
*)
type block = stmt list * expr option
  [@@deriving sexp]

type typevar = Typevar of string
  [@@deriving sexp]

type type_ = 
  | TNamed of string              (* Int         *)
  | TVar of string                (* a           *)
  | TCall of string * type_ list  (* List[a]     *)
  | TFn of type_ * type_          (* x -> y      *)
  | TTuple of type_ list          (* (Int, Bool) *)
  | TUnit                         (* ()          *)
  [@@deriving sexp]

type adt_constructor =
  { name : string;
    arguments : (string option * type_) list
  }
    [@@deriving sexp]

type type_modifier =
  | TNoModifier
  | TPrivate
  | TOpaque
  [@@deriving sexp]

type type_alias_modifier =
  | TANoModifier
  | TAPrivate
  [@@deriving sexp]

type module_modifier =
  | MNoModifier
  | MPrivate
  [@@deriving sexp]

type decl = 
  | DFunction of string * arg_list * expr
  | DTypeAlias of type_alias_modifier * string * typevar list * type_
  | DType of type_modifier * string * typevar list * adt_constructor list
  | DStatement of stmt
  | DModule of module_modifier * string * decl list
  | DExtendModule of Identifier.t * decl list
  | DValueAnnotation of string * type_
  (* TODO if we want to allow type annotations inside statements, it will make
     sense to move DValueAnnotation into DStatement *)
  (* TODO DFunctionAnnotation
          x(y: Int): Bool
   *)
    [@@deriving sexp]

(**************** HELPERS *******************)

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
  | EIdentifier _   -> NoHoles
  | EInt _          -> NoHoles
  | EFloat _        -> NoHoles
  | EChar _         -> NoHoles
  | EString _       -> NoHoles
  | EBool _         -> NoHoles
  | EUnit           -> NoHoles
  | ERecordGetter _ -> NoHoles
  | ETuple es           -> analyze_list es
  | EList es            -> analyze_list es
  | ERecord fs          -> analyze_list (List.map ~f:Tuple2.get2 fs)
  | EConstructor (_,es) -> analyze_list es
  | ELambda (_,e)     -> analyze_holes e
  | EClosure (_,e,_)  -> analyze_holes e
  | EUnOp (_,e)       -> analyze_holes e
  | EPipeline (e1,e2) -> combine_holes (analyze_holes e1) (analyze_holes e2)
  | EBinOp (e1,_,e2)  -> combine_holes (analyze_holes e1) (analyze_holes e2)
  | ECall (fn,args)   -> combine_holes (analyze_holes fn) (analyze_list args)
  | ERecordGet (e,_)  -> analyze_holes e
  | EIf (c,t,e)       -> analyze_list [c;t;e]

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

(*** TO_STRING (IO.println!) ************************************)

let rec expr_to_string env = function
  | EInt i    -> Int.to_string i
  | EFloat f  -> Float.to_string f
                  (* Float.to_string returns 123. instead of 123, so let's strip that *)
                  |> String.rstrip ~drop:(fun c -> Char.equal c '.') 
  | EChar c   -> c
  | EString s -> s
  | EBool b -> string_of_bool b |> String.capitalize
  | EIdentifier (q,x) -> (match Map.find env (q,x) with
      | None -> failwith ("unknown identifier " ^ identifier_to_string (q,x))
      | Some e -> expr_to_string env e
    )
  | EUnit           -> "()"
  | ETuple es       -> "(" ^ (String.concat ~sep:"," (List.map es ~f:(expr_to_string env))) ^ ")"
  | EList es        -> "[" ^ (String.concat ~sep:"," (List.map es ~f:(expr_to_string env))) ^ "]"
  | ERecord fs      -> "{" ^ (String.concat ~sep:"," (List.map fs ~f:(field_to_string env))) ^ "}"
  | EConstructor (name,[]) -> name
  | EConstructor (name,es) -> name ^ "(" ^ String.concat ~sep:"," (List.map es ~f:(expr_to_string env)) ^ ")"
  | EPipeline (e1,e2) -> expr_to_string env e1 ^ " |> " ^ expr_to_string env e2
  | EUnOp _         -> "<unop>"
  | EBinOp _        -> "<binop>"
  | ECall _         -> "<function call>"
  | ELambda _       -> "<function>"
  | EClosure _      -> "<function>"
  | ERecordGet _    -> "<get>"
  | ERecordGetter _ -> "<getter>"
  | EIf _           -> "<if>"

and field_to_string env (f,e) = f ^ ":" ^ expr_to_string env e
