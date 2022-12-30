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

let rec max_hole_n = function
  | EIdentifier ([],x) -> if Char.equal '_' (String.get x 0)
                          then if String.length x = 1
                               then 1 (* _ *)
                               else Int.of_string (String.drop_prefix x 1) (* _3 *)
                          else 0
  | EIdentifier _ -> 0
  | EInt _    -> 0
  | EFloat _  -> 0
  | EChar _   -> 0
  | EString _ -> 0
  | EUnit     -> 0
  | ETuple es -> es |> List.map ~f:max_hole_n |> List.max_elt ~compare |> Option.value ~default:0
  | EList es  -> es |> List.map ~f:max_hole_n |> List.max_elt ~compare |> Option.value ~default:0
  | ELambda (_,e)    -> max_hole_n e
  | EClosure (_,e,_) -> max_hole_n e
  | EUnOp (_,e)      -> max_hole_n e
  | EBinOp (e1,_,e2) -> max (max_hole_n e1) (max_hole_n e2)
  | ECall (fn,args)  -> max (max_hole_n fn) (args |> List.map ~f:max_hole_n |> List.max_elt ~compare |> Option.value ~default:0)

let rec fix_unnumbered_hole e = match e with 
  | EIdentifier ([],"_") -> EIdentifier ([],"_1")
  | EIdentifier _ -> e
  | EInt _    -> e
  | EFloat _  -> e
  | EChar _   -> e
  | EString _ -> e
  | EUnit     -> e
  | ETuple es -> ETuple (es |> List.map ~f:fix_unnumbered_hole)
  | EList es  -> EList  (es |> List.map ~f:fix_unnumbered_hole)
  | ELambda (ns,b)    -> ELambda (ns,fix_unnumbered_hole b)
  | EClosure (ns,b,d) -> EClosure (ns,fix_unnumbered_hole b,d)
  | EUnOp (u,e)      -> EUnOp (u,fix_unnumbered_hole e)
  | EBinOp (e1,b,e2) -> EBinOp (fix_unnumbered_hole e1,b,fix_unnumbered_hole e2)
  | ECall (fn,args)  -> ECall (fix_unnumbered_hole fn,List.map ~f:fix_unnumbered_hole args)

let lambda_with_holes body =
  (* TODO throw a fit if user is mixing _ and _1,_2,... *)
  let max_hole = max_hole_n body in
  let params = List.range 1 (max_hole + 1) |> List.map ~f:(fun i -> "_" ^ Int.to_string i) in
  (* Thanks to already representing the holes as EIdentifiers, we only need to change "_" into "_1" and the rest will all line up! *)
  let new_body = fix_unnumbered_hole body in
  ELambda (params,new_body)