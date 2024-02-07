module AST.Frontend exposing
    ( Bang(..)
    , BangOrExpr(..)
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
    , anyExpr
    , children
    , exprToString
    , functionDefToSingleFunction
    , inspect
    , isEffectfulStmt
    , isSpreadPattern
    , lambdaToString
    , makeRecursive
    , patternToString
    , stmtToString
    , typeToString
    )

import Console
import Debug.Extra
import Env exposing (Env)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)
import NonemptyList exposing (NonemptyList)
import Operator exposing (BinaryOp(..), Operator, UnaryOp(..))
import Transform


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
    | OpIdentifier Operator
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
    | TypeIntrinsic


type TypeAliasModifier
    = TypeAliasNoModifier
    | TypeAliasPrivate


type LetModifier
    = LetNoModifier
    | LetPrivate
    | LetIntrinsic


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
    | DIntrinsicType
        { name : String
        , vars : List String
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


lambdaToString : { args : List Pattern, body : Expr } -> String
lambdaToString { args, body } =
    "(\\{ARGS} -> {BODY})"
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
            exprToString e ++ Operator.unaryOpToString InfiniteRange

        UnaryOp op e ->
            Operator.unaryOpToString op ++ exprToString e

        BinaryOp left op right ->
            "{LEFT} {OP} {RIGHT}"
                |> String.replace "{LEFT}" (exprToString left)
                |> String.replace "{OP}" (Operator.binaryOpToString op)
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

        OpIdentifier op ->
            "({OP})"
                |> String.replace "{OP}" (Operator.toString op)

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

        LetIntrinsic ->
            "intrinsic"


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
    case r.branches of
        ( singleBranch, [] ) ->
            -- TODO we're throwing away fnargs type decls... is that OK?
            { args = List.map Tuple.first singleBranch.args
            , body = singleBranch.body
            }

        _ ->
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


{-| Normally we'd use the Y combinator to achieve recursion.
Elm is eager (strict) though, and using Y combinator would result in an
infinite loop (in the compiler).
(Yes, I've tried. ~janiczek)

Instead, let's use a strict fixed-point combinator Z:
<https://en.wikipedia.org/wiki/Fixed-point_combinator#Strict_fixed-point_combinator>

     Z = \f -> ((\x -> f(\v -> x(x)(v))) (\x -> f(\v -> x(x)(v))))

Theoretically Z is just an eta-expansion of Y. Introduce a lambda and call it
immediately, as in:

     x = someFn(1)
     -- is the same as
     x = (\n -> someFn(n))(1)

-}
zCombinator : Expr
zCombinator =
    let
        pf =
            PVar "f"

        px =
            PVar "x"

        pv =
            PVar "v"

        f =
            Identifier <| Id.simple "f"

        x =
            Identifier <| Id.simple "x"

        v =
            Identifier <| Id.simple "v"

        arm =
            Lambda
                { args = [ px ]
                , body =
                    Call
                        { fn = f
                        , args =
                            [ Lambda
                                { args = [ pv ]
                                , body =
                                    Call
                                        { fn = x
                                        , args = [ x, v ]
                                        }
                                }
                            ]
                        }
                }
    in
    Lambda
        { args = [ pf ]
        , body =
            Call
                { fn = arm
                , args = [ arm ]
                }
        }


{-| The Implementation of Functional Programming Languages (SPJ)
§2.4 Recursive functions

TL;DR the Y-combinator!
Well actually, the Z-combinator (see docs for zCombinator).

    fac = \n -> if n == 0 then 1 else n * fac(n-1)

    ---> beta-abstraction of the self-name

    f = \self -> \n -> if n == 0 then 1 else n * self(n-1)

    (at this point `fac = f(fac)` is true)

    ---> apply Z-combinator

    z = \f -> (\x -> f(\v -> x(x)(v)))(\x -> f(\v -> x(x)(v)))
    fac = z(f)

-}
makeRecursive :
    String
    ->
        { args : List Pattern
        , body : Expr
        }
    ->
        { args : List Pattern
        , body : Expr
        }
makeRecursive name ({ args, body } as old) =
    -- TODO what about root identifiers?
    -- TODO what about qualified identifiers?
    -- TODO do this for normal defs too?
    -- TODO do this for binops too
    -- TODO do this for unary ops too
    let
        realSelf : Expr
        realSelf =
            Identifier (Id.simple name)

        isSelf : Expr -> Bool
        isSelf e =
            e == realSelf
    in
    if anyExpr isSelf body then
        let
            dummySelfStr : String
            dummySelfStr =
                "self"

            dummySelf : Expr
            dummySelf =
                Identifier (Id.simple dummySelfStr)

            replaceWithDummy : Expr -> Expr
            replaceWithDummy e =
                if isSelf e then
                    dummySelf

                else
                    e

            argumentToZ : Expr
            argumentToZ =
                Lambda
                    { args = PVar dummySelfStr :: args
                    , body =
                        Transform.transformOnce
                            recurse
                            replaceWithDummy
                            body
                    }

            newArgs : List String
            newArgs =
                List.range 0 (List.length args - 1)
                    |> List.map (\i -> "arg" ++ String.fromInt i)

            newArgPatterns : List Pattern
            newArgPatterns =
                List.map PVar newArgs

            newArgExprs : List Expr
            newArgExprs =
                List.map (Identifier << Id.simple) newArgs

            newBody : Expr
            newBody =
                Call
                    { fn = zCombinator
                    , args = argumentToZ :: newArgExprs
                    }
        in
        { args = newArgPatterns
        , body = newBody
        }

    else
        old


anyExpr : (Expr -> Bool) -> Expr -> Bool
anyExpr pred expr =
    -- TODO make this streaming somehow in the Transform library?
    children expr
        |> List.any pred


children : Expr -> List Expr
children e =
    Transform.children recursiveChildren e


recurse : (Expr -> Expr) -> Expr -> Expr
recurse f e =
    case e of
        Int _ ->
            e

        Float _ ->
            e

        Char _ ->
            e

        String _ ->
            e

        Bool _ ->
            e

        Unit ->
            e

        Tuple xs ->
            Tuple (List.map f xs)

        List xs ->
            List (List.map f xs)

        Record r ->
            Record (List.map (recurseRecordField f) r)

        UnaryOp op arg ->
            UnaryOp op (f arg)

        BinaryOp l op r ->
            BinaryOp (f l) op (f r)

        Call r ->
            Call
                { fn = f r.fn
                , args = List.map f r.args
                }

        RecordGet r ->
            RecordGet
                { record = f r.record
                , field = r.field
                }

        Block r ->
            Block
                { stmts = List.map (recurseStmt f) r.stmts
                , ret = f r.ret
                }

        EffectBlock r ->
            EffectBlock
                { monadModule = r.monadModule
                , stmts = List.map (recurseStmt f) r.stmts
                , ret = recurseBangOrExpr f r.ret
                }

        Constructor_ r ->
            Constructor_
                { id = r.id
                , args = List.map f r.args
                }

        Identifier _ ->
            e

        RootIdentifier _ ->
            e

        OpIdentifier _ ->
            e

        Lambda r ->
            Lambda
                { args = r.args
                , body = f r.body
                }

        RecordGetter _ ->
            e

        If r ->
            If
                { cond = f r.cond
                , then_ = f r.then_
                , else_ = f r.else_
                }

        Case r ->
            Case
                { subject = f r.subject
                , branches = List.map (recurseCaseBranch f) r.branches
                }


recurseStmt : (Expr -> Expr) -> Stmt -> Stmt
recurseStmt f stmt =
    case stmt of
        SLet r ->
            SLet
                { mod = r.mod
                , lhs = r.lhs
                , type_ = r.type_
                , expr = f r.expr
                }

        SLetBang r ->
            SLetBang
                { mod = r.mod
                , lhs = r.lhs
                , type_ = r.type_
                , bang = recurseBang f r.bang
                }

        SBang bang ->
            SBang (recurseBang f bang)

        SFunctionDef r ->
            SFunctionDef
                { name = r.name
                , mod = r.mod
                , branches = NonemptyList.map (recurseFunctionBranch f) r.branches
                }

        SBinaryOperatorDef r ->
            SBinaryOperatorDef
                { op = r.op
                , left = r.left
                , right = r.right
                , body = f r.body
                }

        SUnaryOperatorDef r ->
            SUnaryOperatorDef
                { op = r.op
                , arg = r.arg
                , body = f r.body
                }

        SValueAnnotation _ ->
            stmt

        SBinaryOperatorAnnotation _ ->
            stmt

        SUnaryOperatorAnnotation _ ->
            stmt

        SUseModule _ ->
            stmt


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

        Record r ->
            List.filterMap recordFieldExpr r
                |> List.concatMap f

        UnaryOp op arg ->
            f arg

        BinaryOp l op r ->
            f l ++ f r

        Call r ->
            f r.fn ++ List.concatMap f r.args

        RecordGet r ->
            f r.record

        Block r ->
            List.concatMap (stmtChildren f) r.stmts ++ f r.ret

        EffectBlock r ->
            List.concatMap (stmtChildren f) r.stmts ++ bangOrExprChildren f r.ret

        Constructor_ { args } ->
            List.concatMap f args

        Identifier _ ->
            []

        RootIdentifier _ ->
            []

        OpIdentifier _ ->
            []

        Lambda r ->
            f r.body

        RecordGetter _ ->
            []

        If r ->
            f r.cond ++ f r.then_ ++ f r.else_

        Case r ->
            f r.subject ++ List.concatMap (.body >> f) r.branches


stmtChildren : (Expr -> List Expr) -> Stmt -> List Expr
stmtChildren f stmt =
    case stmt of
        SLet r ->
            f r.expr

        SLetBang r ->
            bangChildren f r.bang

        SBang bang ->
            bangChildren f bang

        SFunctionDef r ->
            r.branches
                |> NonemptyList.toList
                |> List.concatMap (.body >> f)

        SBinaryOperatorDef r ->
            f r.body

        SUnaryOperatorDef r ->
            f r.body

        SValueAnnotation _ ->
            []

        SBinaryOperatorAnnotation _ ->
            []

        SUnaryOperatorAnnotation _ ->
            []

        SUseModule _ ->
            []


bangChildren : (Expr -> List Expr) -> Bang -> List Expr
bangChildren f bang =
    case bang of
        BValue expr ->
            f expr

        BCall r ->
            f r.fn ++ List.concatMap f r.args


bangOrExprChildren : (Expr -> List Expr) -> BangOrExpr -> List Expr
bangOrExprChildren f boe =
    case boe of
        B bang ->
            bangChildren f bang

        E expr ->
            f expr


recurseCaseBranch : (Expr -> Expr) -> CaseBranch -> CaseBranch
recurseCaseBranch f branch =
    { pattern = branch.pattern
    , body = f branch.body
    }


recurseRecordField : (Expr -> Expr) -> RecordExprContent -> RecordExprContent
recurseRecordField f content =
    case content of
        Field r ->
            Field
                { field = r.field
                , expr = f r.expr
                }

        Pun _ ->
            content

        Spread _ ->
            content


recurseFunctionBranch : (Expr -> Expr) -> FunctionBranch -> FunctionBranch
recurseFunctionBranch f branch =
    { args = branch.args
    , body = f branch.body
    }


recurseBangOrExpr : (Expr -> Expr) -> BangOrExpr -> BangOrExpr
recurseBangOrExpr f boe =
    case boe of
        B bang ->
            B (recurseBang f bang)

        E expr ->
            E (f expr)


recurseBang : (Expr -> Expr) -> Bang -> Bang
recurseBang f bang =
    case bang of
        BValue expr ->
            BValue (f expr)

        BCall r ->
            BCall
                { fn = f r.fn
                , args = List.map f r.args
                }


recordFieldExpr : RecordExprContent -> Maybe Expr
recordFieldExpr content =
    case content of
        Field { expr } ->
            Just expr

        Pun _ ->
            Nothing

        Spread _ ->
            Nothing
