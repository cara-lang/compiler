module AST.Frontend exposing
    ( Bang(..)
    , BangOrExpr(..)
    , BinaryOp(..)
    , CaseBranch
    , Decl(..)
    , Expr(..)
    , FunctionBranch
    , LetModifier(..)
    , ModuleModifier(..)
    , Pattern(..)
    , Program
    , RecordExprContent(..)
    , RecordTypeField
    , Stmt(..)
    , Type(..)
    , TypeAliasModifier(..)
    , TypeConstructor
    , TypeConstructorArg
    , TypeModifier(..)
    , UnaryOp(..)
    , binaryOp
    , functionDefToSingleFunction
    , inspect
    , isEffectfulStmt
    , isSpreadPattern
    , lambdaToString
    , patternToString
    , stmtToString
    , typeToString
    , unaryOp
    )

import Debug.Extra
import Env exposing (Env)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)
import NonemptyList exposing (NonemptyList)


type alias Program =
    List Decl


type Expr
    = -- Literals
      Int Int
    | Float Float
    | Char String -- Holding a string because of extended grapheme clusters
    | String String
    | Bool Bool -- TODO define this as Constructor instead?
    | Unit
      -- Collections
    | Tuple (List Expr) -- (1,True)
    | List (List Expr) -- [1,2]
    | Record (List RecordExprContent) -- {a:1, ...b, c:True}
      -- Calls
    | UnaryOp UnaryOp Expr -- ~a
    | BinaryOp Expr BinaryOp Expr -- a+b
    | Call
        -- foo(), bar(1,2)
        { fn : Expr
        , args : List Expr
        }
    | RecordGet
        -- record.field
        { record : Expr
        , field : String
        }
      -- Blocks
    | Block
        -- x = { ... }
        { stmts : List Stmt
        , ret : Expr
        }
    | EffectBlock
        -- x = My.Monad { ... }
        { monadModule : List String
        , stmts : List Stmt
        , ret : BangOrExpr
        }
      -- Other
    | Constructor_
        -- Foo, Bar.Foo, Foo(1,2), Bar.Foo(1,2)
        { id : Id
        , args : List Expr
        }
    | Identifier Id -- foo, Bar.foo
    | RootIdentifier Id -- ::foo, ::Bar.foo
    | Lambda
        { args : List Pattern
        , body : Expr
        }
    | RecordGetter String -- .field
    | If
        { cond : Expr
        , then_ : Expr
        , else_ : Expr
        }
    | Case
        { subject : Expr
        , branches : List CaseBranch
        }


type Type
    = TNamed Id -- Int, Base.Maybe
    | TApplication { type_ : Type, args : List Type } -- List[a]
    | TVar String -- a
    | TFn { from : Type, to : Type } -- x -> y
    | TThunk { to : Type } -- -> x
    | TTuple (List Type) -- (Int, Bool)
    | TRecord (List RecordTypeField) -- {a:Int,b:Bool}
    | TUnit -- ()


type Pattern
    = PUnit -- ()
    | PVar String -- a
    | PConstructor { id : Id, args : List Pattern } -- Foo, Bar.Foo, Foo(a), Foo(_), Foo([])
    | PInt Int -- 1
    | PFloat Float -- 1.2345
    | PChar String -- 'a'
    | PString String -- "abc"
    | PBool Bool -- True
    | PList (List Pattern) -- [1,a]
    | PTuple (List Pattern) -- (1,a)
    | PWildcard -- _
    | PSpread (Maybe String) -- ...a, ..._
    | PRecordSpread -- {..}
    | PRecordFields (List String) -- {a}, {a,b}
    | PAs String Pattern
      {- These aren't ever parsed, but they're a way for interpretEffectBlock
         to create an expression out of SUnaryOperatorDef and
         SBinaryOperatorDef statements.
      -}
    | PUnaryOpDef UnaryOp
    | PBinaryOpDef BinaryOp
    | POr Pattern Pattern -- a | b


type Bang
    = BValue Expr -- foo!, Bar.foo!, x |> foo!, foo.bar!
    | BCall
        -- foo!(1,2), Bar.foo!(1,2), x |> foo!(1,2), foo.bar!(1,2)
        { fn : Expr
        , args : List Expr
        }


type BangOrExpr
    = B Bang
    | E Expr


type Stmt
    = SLet
        { mod : LetModifier
        , lhs : Pattern
        , type_ : Maybe Type
        , expr : Expr
        }
    | SLetBang
        { mod : LetModifier
        , lhs : Pattern
        , type_ : Maybe Type
        , bang : Bang
        }
    | SBang Bang
    | SFunctionDef
        { name : String
        , mod : LetModifier
        , branches : NonemptyList FunctionBranch
        }
    | SBinaryOperatorDef
        { op : BinaryOp
        , left : ( Pattern, Maybe Type ) -- TODO maybe always need type here?
        , right : ( Pattern, Maybe Type ) -- TODO maybe always need type here?
        , body : Expr
        }
    | SUnaryOperatorDef
        { op : UnaryOp
        , arg : ( Pattern, Maybe Type ) -- TODO maybe always need type here?
        , body : Expr
        }
    | SValueAnnotation
        { mod : LetModifier
        , name : String
        , type_ : Type
        }
    | SBinaryOperatorAnnotation
        { mod : LetModifier
        , op : BinaryOp
        , left : Type
        , right : Type
        , ret : Type
        }
    | SUnaryOperatorAnnotation
        { mod : LetModifier
        , op : UnaryOp
        , arg : Type
        , ret : Type
        }
    | SUseModule Id


type alias FunctionBranch =
    { args : List ( Pattern, Maybe Type )
    , body : Expr
    }


type TypeModifier
    = TypeNoModifier
    | TypePrivate
    | TypeOpaque


type TypeAliasModifier
    = TypeAliasNoModifier
    | TypeAliasPrivate


type LetModifier
    = LetNoModifier
    | LetPrivate


type Decl
    = DTypeAlias
        { mod : TypeAliasModifier
        , name : String
        , vars : List String
        , body : Type
        }
    | DType
        { mod : TypeModifier
        , name : String
        , vars : List String
        , constructors : List TypeConstructor
        }
    | DModule
        { mod : ModuleModifier
        , name : String
        , decls : List Decl
        }
    | DExtendModule
        { module_ : List String
        , decls : List Decl
        }
    | DStatement Stmt
    | DUnitTest
        { name : Maybe String
        , expr : Expr
        }
    | DParameterizedTest
        { name : Maybe String
        , table : List Expr
        , -- implicit lambda
          args : List Pattern
        , expr : Expr
        }
    | DPropertyTypeTest
        { name : Maybe String
        , types : Type
        , -- implicit lambda
          args : List Pattern
        , expr : Expr
        }
    | DPropertyGenTest
        { name : Maybe String
        , gens : Expr
        , -- implicit lambda
          args : List Pattern
        , expr : Expr
        }


type ModuleModifier
    = ModuleNoModifier
    | ModulePrivate


type alias CaseBranch =
    -- 1 -> "hello"
    { pattern : Pattern
    , body : Expr
    }


type RecordExprContent
    = Field { field : String, expr : Expr } -- a:123
    | Pun String -- a
    | Spread Id -- ...b


type alias RecordTypeField =
    -- a:Int
    { field : String
    , type_ : Type
    }


type alias TypeConstructor =
    -- Foo
    -- Bar(Int)
    -- Baz(n: Int, verbose: Bool)
    { name : String
    , args : List TypeConstructorArg
    }


type alias TypeConstructorArg =
    -- a:Int
    -- Int
    { name : Maybe String
    , type_ : Type
    }


type UnaryOp
    = NegateNum -- -e
    | NegateBool -- !e
    | NegateBin -- ~e
    | InfiniteRange -- e..


type BinaryOp
    = -- arithmetic
      Plus -- +
    | Minus -- -
    | Times -- *
    | Div -- /
    | Mod -- %
    | Pow -- **
      -- binary
    | OrBin -- |
    | AndBin -- &
    | XorBin -- ^
    | ShiftL -- <<
    | ShiftR -- >>
    | ShiftRU -- >>>, unsigned
      -- comparisons and booleans
    | Lte -- <=
    | Lt -- <
    | Eq -- ==
    | Neq -- !=
    | Gt -- >
    | Gte -- >=
    | OrBool -- ||
    | AndBool -- &&
      -- appendables
    | Append -- ++
      -- ranges
    | RangeInclusive -- ..
    | RangeExclusive -- ...


isEffectfulStmt : Stmt -> Bool
isEffectfulStmt stmt =
    case stmt of
        SLetBang _ ->
            True

        SBang _ ->
            True

        --
        SLet _ ->
            False

        SFunctionDef _ ->
            False

        SBinaryOperatorDef _ ->
            False

        SUnaryOperatorDef _ ->
            False

        SValueAnnotation _ ->
            False

        SBinaryOperatorAnnotation _ ->
            False

        SUnaryOperatorAnnotation _ ->
            False

        SUseModule _ ->
            False


isSpreadPattern : Pattern -> Bool
isSpreadPattern pattern =
    case pattern of
        PSpread _ ->
            True

        PAs _ inner ->
            isSpreadPattern inner

        PUnit ->
            False

        PVar _ ->
            False

        PConstructor _ ->
            False

        PInt _ ->
            False

        PFloat _ ->
            False

        PChar _ ->
            False

        PString _ ->
            False

        PBool _ ->
            False

        PList _ ->
            False

        PTuple _ ->
            False

        PWildcard ->
            False

        PRecordSpread ->
            False

        PRecordFields _ ->
            False

        PUnaryOpDef _ ->
            False

        PBinaryOpDef _ ->
            False

        POr _ _ ->
            False


binaryOp : BinaryOp -> String
binaryOp op =
    case op of
        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "*"

        Div ->
            "/"

        Mod ->
            "%"

        Pow ->
            "**"

        OrBin ->
            "|"

        AndBin ->
            "&"

        XorBin ->
            "^"

        ShiftL ->
            "<<"

        ShiftR ->
            ">>"

        ShiftRU ->
            ">>>"

        Lte ->
            "<="

        Lt ->
            "<"

        Eq ->
            "=="

        Neq ->
            "!="

        Gt ->
            ">"

        Gte ->
            ">="

        OrBool ->
            "||"

        AndBool ->
            "&&"

        Append ->
            "++"

        RangeInclusive ->
            ".."

        RangeExclusive ->
            "..."


unaryOp : UnaryOp -> String
unaryOp op =
    case op of
        NegateNum ->
            "-"

        NegateBool ->
            "!"

        NegateBin ->
            "~"

        InfiniteRange ->
            ".."


lambdaToString : { args : List Pattern, body : Expr } -> String
lambdaToString { args, body } =
    "\\{ARGS} -> {BODY}"
        |> String.replace "{ARGS}" (String.join "," (List.map patternToString args))
        |> String.replace "{BODY}" (exprToString body)


patternWithTypeToString : ( Pattern, Maybe Type ) -> String
patternWithTypeToString ( pattern, maybeType ) =
    case maybeType of
        Nothing ->
            patternToString pattern

        Just type_ ->
            "{PATTERN} : {TYPE}"
                |> String.replace "{PATTERN}" (patternToString pattern)
                |> String.replace "{TYPE}" (typeToString type_)


patternToString : Pattern -> String
patternToString pattern =
    case pattern of
        PUnit ->
            "()"

        PVar name ->
            name

        PConstructor { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "("
                            ++ String.join "," (List.map patternToString args)
                            ++ ")"
                   )

        PInt n ->
            String.fromInt n

        PFloat n ->
            String.fromFloat n

        PChar s ->
            "'{CHAR}'"
                -- TODO escaping?
                |> String.replace "{CHAR}" s

        PString s ->
            "\"{STR}\""
                -- TODO escaping?
                |> String.replace "{STR}" s

        PBool bool ->
            if bool then
                "True"

            else
                "False"

        PAs name inner ->
            "({INNER}) as {NAME}"
                |> String.replace "{NAME}" name
                |> String.replace "{INNER}" (patternToString inner)

        PList xs ->
            "[{LIST}]"
                |> String.replace "{LIST}" (String.join "," (List.map patternToString xs))

        PTuple xs ->
            "({TUPLE})"
                |> String.replace "{TUPLE}" (String.join "," (List.map patternToString xs))

        PWildcard ->
            "_"

        PSpread val ->
            "..." ++ Maybe.withDefault "_" val

        PRecordSpread ->
            "{..}"

        PRecordFields fields ->
            "{|RECORD|}"
                |> String.replace "|RECORD|" (String.join "," fields)

        PUnaryOpDef _ ->
            "<UNARY OP DEF>"

        PBinaryOpDef _ ->
            "<BINARY OP DEF>"

        POr l r ->
            "{L} | {R}"
                |> String.replace "{L}" (patternToString l)
                |> String.replace "{R}" (patternToString r)


recordExprContentToString : RecordExprContent -> String
recordExprContentToString content =
    case content of
        Field { field, expr } ->
            field ++ ":" ++ exprToString expr

        Pun field ->
            field

        Spread id ->
            "..." ++ Id.toString id


exprToString : Expr -> String
exprToString expr =
    case expr of
        Int n ->
            String.fromInt n

        Float n ->
            String.fromFloat n

        Char c ->
            "'" ++ c ++ "'"

        String s ->
            "\"" ++ s ++ "\""

        Bool b ->
            if b then
                "True"

            else
                "False"

        Unit ->
            "()"

        Tuple xs ->
            "(" ++ String.join "," (List.map exprToString xs) ++ ")"

        List xs ->
            "[" ++ String.join "," (List.map exprToString xs) ++ "]"

        Record contents ->
            "{" ++ String.join "," (List.map recordExprContentToString contents) ++ "}"

        UnaryOp InfiniteRange e ->
            exprToString e ++ unaryOp InfiniteRange

        UnaryOp op e ->
            unaryOp op ++ exprToString e

        BinaryOp left op right ->
            "{LEFT} {OP} {RIGHT}"
                |> String.replace "{LEFT}" (exprToString left)
                |> String.replace "{OP}" (binaryOp op)
                |> String.replace "{RIGHT}" (exprToString right)

        Call { fn, args } ->
            exprToString fn
                ++ "("
                ++ String.join "," (List.map exprToString args)
                ++ ")"

        RecordGet { record, field } ->
            exprToString record ++ "." ++ field

        Block { stmts, ret } ->
            [ [ "{" ]
            , List.map (stmtToString >> indent4) stmts
            , [ exprToString ret
              , "}"
              ]
            ]
                |> List.concat
                |> String.join "\n"

        EffectBlock r ->
            [ [ String.join "." r.monadModule ++ " {" ]
            , List.map (stmtToString >> indent4) r.stmts
            , [ bangOrExprToString r.ret
              , "}"
              ]
            ]
                |> List.concat
                |> String.join "\n"

        Constructor_ { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "("
                            ++ String.join "," (List.map exprToString args)
                            ++ ")"
                   )

        Identifier id ->
            Id.toString id

        RootIdentifier id ->
            "::" ++ Id.toString id

        Lambda r ->
            lambdaToString r

        RecordGetter field ->
            "." ++ field

        If r ->
            "if {COND} then {THEN} else {ELSE}"
                |> String.replace "{COND}" (exprToString r.cond)
                |> String.replace "{THEN}" (exprToString r.then_)
                |> String.replace "{ELSE}" (exprToString r.else_)

        Case r ->
            "case {SUBJECT} of\n{BRANCHES}"
                |> String.replace "{SUBJECT}" (exprToString r.subject)
                |> String.replace "{BRANCHES}" (String.join "\n" (List.map (caseBranchToString >> indent2) r.branches))


bangOrExprToString : BangOrExpr -> String
bangOrExprToString boe =
    case boe of
        B bang ->
            bangToString bang

        E expr ->
            exprToString expr


bangToString : Bang -> String
bangToString bang =
    case bang of
        BValue expr ->
            "{EXPR}!"
                |> String.replace "{EXPR}" (exprToString expr)

        BCall { fn, args } ->
            "{FN}!({ARGS})"
                |> String.replace "{FN}" (exprToString fn)
                |> String.replace "{ARGS}"
                    (String.join ","
                        (List.map exprToString args)
                    )


caseBranchToString : CaseBranch -> String
caseBranchToString caseBranch =
    "{PATTERN} -> {BODY}"
        |> String.replace "{PATTERN}" (patternToString caseBranch.pattern)
        |> String.replace "{BODY}" (exprToString caseBranch.body)


stmtToString : Stmt -> String
stmtToString stmt =
    case stmt of
        SLet r ->
            "{MOD}{PATTERN}{TYPE} = {VALUE}"
                |> String.replace "{MOD}"
                    (letModifierToString r.mod
                        |> withSpaceToRightIfNotEmpty
                    )
                |> String.replace "{PATTERN}" (patternToString r.lhs)
                |> String.replace "{TYPE}"
                    (case r.type_ of
                        Nothing ->
                            ""

                        Just type_ ->
                            typeToString type_
                    )
                |> String.replace "{VALUE}" (exprToString r.expr)

        SLetBang r ->
            "{MOD}{PATTERN}{TYPE} = {BANG}"
                |> String.replace "{MOD}"
                    (letModifierToString r.mod
                        |> withSpaceToRightIfNotEmpty
                    )
                |> String.replace "{PATTERN}" (patternToString r.lhs)
                |> String.replace "{TYPE}"
                    (case r.type_ of
                        Nothing ->
                            ""

                        Just type_ ->
                            typeToString type_
                    )
                |> String.replace "{BANG}" (bangToString r.bang)

        SBang bang ->
            bangToString bang

        SFunctionDef r ->
            r.branches
                |> NonemptyList.toList
                |> List.map
                    (\branch ->
                        "{MOD}{NAME}({ARGS}) = {BODY}"
                            |> String.replace "{MOD}"
                                (letModifierToString r.mod
                                    |> withSpaceToRightIfNotEmpty
                                )
                            |> String.replace "{NAME}" r.name
                            |> String.replace "{ARGS}" (String.join "," (List.map patternWithTypeToString branch.args))
                            |> String.replace "{BODY}" (exprToString branch.body)
                    )
                |> String.join "\n"

        SBinaryOperatorDef r ->
            Debug.Extra.todo1 "stmtToString: SBinaryOperatorDef" r

        SUnaryOperatorDef r ->
            Debug.Extra.todo1 "stmtToString: SUnaryOperatorDef" r

        SValueAnnotation r ->
            "{MOD}{NAME} : {TYPE}"
                |> String.replace "{MOD}"
                    (letModifierToString r.mod
                        |> withSpaceToRightIfNotEmpty
                    )
                |> String.replace "{NAME}" r.name
                |> String.replace "{TYPE}" (typeToString r.type_)

        SBinaryOperatorAnnotation r ->
            Debug.Extra.todo1 "stmtToString: SBinaryOperatorAnnotation" r

        SUnaryOperatorAnnotation r ->
            Debug.Extra.todo1 "stmtToString: SUnaryOperatorAnnotation" r

        SUseModule id ->
            Debug.Extra.todo1 "stmtToString: SUseModule" id


letModifierToString : LetModifier -> String
letModifierToString mod =
    case mod of
        LetNoModifier ->
            ""

        LetPrivate ->
            "private"


withSpaceToRightIfNotEmpty : String -> String
withSpaceToRightIfNotEmpty s =
    if s == "" then
        s

    else
        s ++ " "


indent4 : String -> String
indent4 str =
    spaces 4 ++ str


indent2 : String -> String
indent2 str =
    spaces 2 ++ str


spaces : Int -> String
spaces n =
    String.repeat n " "


inspect : Program -> String
inspect decls =
    Debug.Extra.prettyPrint decls


typeToString : Type -> String
typeToString t =
    case t of
        TNamed id ->
            Id.toString id

        TApplication { type_, args } ->
            --List[a]
            "{TYPE}[{ARGS}]"
                |> String.replace "{TYPE}" (typeToString type_)
                |> String.replace "{ARGS}" (String.join "," (List.map typeToString args))

        TVar var ->
            var

        TFn { from, to } ->
            "{FROM} -> {TO}"
                |> String.replace "{FROM}" (typeToString from)
                |> String.replace "{TO}" (typeToString to)

        TThunk { to } ->
            "-> {TO}"
                |> String.replace "{TO}" (typeToString to)

        TTuple xs ->
            "({TYPES})"
                |> String.replace "{TYPES}" (String.join ", " (List.map typeToString xs))

        TRecord fields ->
            "{|FIELDS|}"
                |> String.replace "|FIELDS|" (String.join ", " (List.map recordTypeFieldToString fields))

        TUnit ->
            "()"


recordTypeFieldToString : RecordTypeField -> String
recordTypeFieldToString field =
    "{FIELD}:{TYPE}"
        |> String.replace "{FIELD}" field.field
        |> String.replace "{TYPE}" (typeToString field.type_)


{-|

    foo([]) = 1
    foo([x]) = 2
    foo([x,...xs]) = 5
    -->
    foo(a) =
        case a of
            [] -> 1
            [x] -> 2
            [x,...xs] -> 5

-}
functionDefToSingleFunction :
    { name : String
    , mod : LetModifier
    , branches : NonemptyList FunctionBranch
    }
    ->
        { args : List Pattern
        , body : Expr
        }
functionDefToSingleFunction r =
    let
        argCount =
            NonemptyList.head r.branches
                |> .args
                |> List.length

        vars =
            List.range 0 (argCount - 1)
                |> List.map (\i -> "arg" ++ String.fromInt i)
    in
    { args = List.map PVar vars
    , body =
        Case
            { subject = Tuple (List.map (Identifier << Id.simple) vars)
            , branches =
                r.branches
                    |> NonemptyList.toList
                    |> List.map
                        (\branch ->
                            { pattern = PTuple (List.map Tuple.first branch.args)
                            , body = branch.body
                            }
                        )
            }
    }
