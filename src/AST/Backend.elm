module AST.Backend exposing
    ( Decl(..)
    , Expr(..)
    , Pattern(..)
    , Program
    , TypeConstructor
    , getExprs
    , getRecord
    , getTuple
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


getExprs : (Expr -> Maybe a) -> Program -> List a
getExprs fn decls =
    foldExprs
        (\expr acc ->
            case fn expr of
                Nothing ->
                    acc

                Just data ->
                    data :: acc
        )
        []
        decls


foldExprs : (Expr -> a -> a) -> a -> Program -> a
foldExprs fn init decls =
    List.foldl
        (\decl acc -> foldDecl fn acc decl)
        init
        decls


foldDecl : (Expr -> a -> a) -> a -> Decl -> a
foldDecl fn init decl =
    case decl of
        DType _ ->
            init

        DLetStmt r ->
            foldExpr fn init r.expr

        DFunctionDef r ->
            foldExpr fn init r.body


foldExpr : (Expr -> a -> a) -> a -> Expr -> a
foldExpr fn init expr =
    let
        self () =
            fn expr init

        selfAnd xs =
            List.foldl fn (self ()) xs
    in
    case expr of
        Int _ ->
            self ()

        Float _ ->
            self ()

        Char _ ->
            self ()

        String _ ->
            self ()

        Bool _ ->
            self ()

        Unit ->
            self ()

        Tuple xs ->
            selfAnd xs

        List xs ->
            selfAnd xs

        Constructor_ { args } ->
            selfAnd args

        Lambda1 { body } ->
            selfAnd [ body ]

        RootIdentifier _ ->
            self ()

        If r ->
            selfAnd [ r.cond, r.then_, r.else_ ]

        FnCall1 r ->
            selfAnd [ r.fn, r.arg ]

        Let1 r ->
            selfAnd [ r.value, r.body ]

        RecordGetter _ ->
            self ()

        Record r ->
            selfAnd (List.map .expr r.sortedFields)


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
