module HVM.AST exposing
    ( BinOp(..)
    , File
    , Rule
    , Term(..)
    )


type alias File =
    -- TODO what about SMaps? Would it be helpful to us to make some things strict?
    { rules : List Rule
    }


type alias Rule =
    { lhs : Term
    , rhs : Term
    }


type BinOp
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | And
    | Or
    | Xor
    | Shl
    | Shr
    | Lte
    | Ltn
    | Eql
    | Gte
    | Gtn
    | Neq


type Term
    = Var String
    | Dup { leftName : String, rightName : String, expr : Term, body : Term }
    | Sup { left : Term, right : Term }
    | Let { name : String, expr : Term, body : Term }
    | Lam { name : String, body : Term }
    | App { function : Term, arg : Term }
    | Ctr { name : String, args : List Term }
    | U6O Int
    | F6O Float
    | Op2 { op : BinOp, left : Term, right : Term }
