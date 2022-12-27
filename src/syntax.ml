type unop = OpNeg

type binop = OpPlus | OpMinus | OpTimes | OpDiv

and expr =
  | EInt of int
  | EString of string
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr
  | EIoPrintln of expr

type value =
  | VInt of int
  | VString of string
  | VUnit
  [@@deriving show, map]