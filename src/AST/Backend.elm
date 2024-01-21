module AST.Backend exposing
    ( Decl(..)
    , Expr(..)
    , Pattern(..)
    , Program
    , TypeConstructor
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
    | Constructor_
        { id : QualifiedId
        , args : List Expr
        }
    | Lambda1
        { arg : String
        , body : Expr
        }
    | RootIdentifier QualifiedId
    | If
        { cond : Expr
        , then_ : Expr
        , else_ : Expr
        }
    | FnCall1
        { fn : Expr
        , arg : Expr
        }
    | Let1
        { name : String
        , value : Expr
        , body : Expr
        }
    | RecordGetter String


type Decl
    = DType
        { id : QualifiedId
        , vars : List String
        , constructors : List TypeConstructor
        }
    | DLetStmt
        { lhs : Pattern
        , expr : Expr
        }
    | DFunctionDef
        { id : QualifiedId
        , args : List Pattern
        , body : Expr
        }


type Pattern
    = PUnit
    | PVar String
    | PConstructor
        { id : QualifiedId
        , args : List Pattern
        }
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


type alias TypeConstructor =
    { id : QualifiedId
    , arity : Int -- HVM won't use the name/type information
    }
