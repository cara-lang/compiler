type unop = OpNeg

type binop = OpPlus | OpMinus | OpTimes | OpDiv

(*
and stmt =
  | SLet of string * expr
  | SLetBang of string * expr * expr
  | SBang of expr * expr
  (* note we don't support non-! version of SLet as we are pure and it would just spin CPU! *)
*)

and expr =
  (* literals *)
  | EInt of int
  | EString of string
  | EUnit

  (* kludges *)
  | EIoPrintln
  
  (* calls *)
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr
  | ECall of expr * expr list