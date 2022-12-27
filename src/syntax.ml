type unop = 
  | OpNeg

type binop =
  | OpPlus 
  | OpMinus 
  | OpTimes 
  | OpDiv

type expr =
  (* literals *)
  | EInt of int (* 123 *)
  | EString of string (* "abc" *)
  | EUnit
  | ETuple of expr list (* (1,"abc") or (1,"abc",2,()) *)

  (* kludges *)
  | EIoPrintln (* IO.println *)
  
  (* calls *)
  | EUnOp of unop * expr (* -num *)
  | EBinOp of expr * binop * expr (* 1 + 2 *)
  | ECall of expr * expr list (* foo(1,2,3) *)

and stmt_list = 
  | StmtList of stmt list * expr option
  (* The last expr is the returned one. If None, we return Unit.
     These get chained together:
        x = 123 ----> pure 123 >>= \x  -> ...
        x = foo! ---> foo      >>= \x  -> ...
        foo! -------> foo      >>= \() -> ...
        123 --------> (only allowed in the last position) 123
  *)

and stmt =
  | SLet of string * expr (* x = 123 *)
  | SLetBang of string * bang (* x = foo! *)
  | SBang of bang (* foo! *)

and bang =
  | BValue of expr (* foo! *)
  | BCall of expr * expr list (* foo!(1,2,3) *)
