module AST.Backend exposing
    ( Constructor
    , Decl(..)
    , Expr(..)
    , Pattern(..)
    , Program
    )

import Id.Qualified exposing (QualifiedId)


type alias Program =
    List Decl


type Expr
    = Int Int
    | Float Float
    | Char String
    | String String
    | Bool Bool
    | Unit
    | Tuple (List Expr)
    | List (List Expr)
    | Constructor_ { id : QualifiedId, args : List Expr }
    | Lambda1 { arg : String, body : Expr }
    | RootIdentifier QualifiedId
    | If { cond : Expr, then_ : Expr, else_ : Expr }
    | FnCall1 { fn : Expr, arg : Expr }
    | Let1 { name : String, value : Expr, body : Expr }


type Decl
    = DType { id : QualifiedId, vars : List String, constructors : List Constructor }
    | DLetStmt { lhs : Pattern, expr : Expr }
    | DIoStmt Expr


type Pattern
    = PUnit
    | PVar String
    | PConstructor { id : QualifiedId, args : List Pattern }
    | PInt Int
    | PFloat Float
    | PChar String
    | PString String
    | PList (List Pattern)
    | PTuple (List Pattern)
    | PWildcard
    | PSpread (Maybe String)
    | PRecordSpread
    | PRecordFields (List String)
    | PAs String Pattern


type alias Constructor =
    { id : QualifiedId
    , argsCount : Int -- HVM won't use the name/type information
    }
