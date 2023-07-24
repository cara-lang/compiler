module AST.Lowered exposing (Expr(..))

import Id exposing (Id)


type Expr
    = Int Int
    | Float Float
    | Char String
    | String String
    | Bool Bool
    | Unit
    | Tuple (List Expr)
    | List (List Expr)
    | Constructor { id : Id, args : List Expr }
    | Lambda1 { arg : String, body : Expr }
    | RootIdentifier Id
    | If { cond : Expr, then_ : Expr, else_ : Expr }
    | FnCall1 { fn : Expr, arg : Expr }
    | Let1 { name : String, value : Expr, body : Expr }
