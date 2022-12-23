type unop = OpNeg

type binop = OpPlus | OpMinus | OpTimes | OpDiv

and expr =
  | ELiteral of int
  | EUnOp of unop * expr
  | EBinOp of expr * binop * expr
