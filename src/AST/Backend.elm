module AST.Backend exposing
    ( Decl(..)
    , Expr(..)
    , Pattern(..)
    , Program
    , TypeConstructor
    , children
    , getRecord
    , getTuple
    , programChildren
    )

import Id.Qualified exposing (QualifiedId)
import Transform


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
    | Record
        { sortedFields :
            List
                { field : String
                , expr : Expr
                }
        }


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
    | PBool Bool
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


children : Expr -> List Expr
children e =
    Transform.children recursiveChildren e


programChildren : Program -> List Expr
programChildren decls =
    List.concatMap declChildren decls


declChildren : Decl -> List Expr
declChildren decl =
    case decl of
        DType _ ->
            []

        DLetStmt r ->
            children r.expr

        DFunctionDef r ->
            children r.body


recursiveChildren : (Expr -> List Expr) -> Expr -> List Expr
recursiveChildren f e =
    case e of
        Int _ ->
            []

        Float _ ->
            []

        Char _ ->
            []

        String _ ->
            []

        Bool _ ->
            []

        Unit ->
            []

        Tuple xs ->
            List.concatMap f xs

        List xs ->
            List.concatMap f xs

        Constructor_ r ->
            List.concatMap f r.args

        Lambda1 r ->
            f r.body

        RootIdentifier _ ->
            []

        If r ->
            f r.cond ++ f r.then_ ++ f r.else_

        FnCall1 r ->
            f r.fn ++ f r.arg

        Let1 r ->
            f r.value ++ f r.body

        RecordGetter _ ->
            []

        Record r ->
            List.concatMap (.expr >> f) r.sortedFields


getTuple : Expr -> Maybe (List Expr)
getTuple e =
    case e of
        Tuple xs ->
            Just xs

        _ ->
            Nothing


getRecord : Expr -> Maybe { sortedFields : List { field : String, expr : Expr } }
getRecord e =
    case e of
        Record r ->
            Just r

        _ ->
            Nothing
