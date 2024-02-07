module Desugar exposing (desugarProgram)

import AST.Backend as B
import AST.Frontend as F
import Debug.Extra
import Env
import Error exposing (DesugarError)
import Id exposing (Id)
import Id.Qualified exposing (QualifiedId)
import NonemptyList exposing (NonemptyList)
import Result.Extra as Result


desugarProgram : F.Program -> Result DesugarError B.Program
desugarProgram decls =
    decls
        |> Result.combineMap desugarDecl
        |> Result.map List.concat


desugarDecl : F.Decl -> Result DesugarError (List B.Decl)
desugarDecl decl =
    case decl of
        F.DTypeAlias r ->
            Debug.Extra.todo1 "desugar type alias" r

        F.DType r ->
            Ok
                -- TODO mod
                [ B.DType
                    { id = qualify (Id.simple r.name)
                    , vars = r.vars
                    , constructors = List.map desugarTypeConstructor r.constructors
                    }
                ]

        F.DIntrinsicType r ->
            Debug.Extra.todo1 "desugar intrinsic type" r

        F.DModule r ->
            Debug.Extra.todo1 "desugar module" r

        F.DExtendModule r ->
            Debug.Extra.todo1 "desugar extend module" r

        F.DStatement stmt ->
            desugarStmt stmt

        F.DUnitTest r ->
            Debug.Extra.todo1 "desugar unit test" r

        F.DParameterizedTest r ->
            Debug.Extra.todo1 "desugar parameterized test" r

        F.DPropertyTypeTest r ->
            Debug.Extra.todo1 "desugar property type test" r

        F.DPropertyGenTest r ->
            Debug.Extra.todo1 "desugar property gen test" r


desugarTypeConstructor : F.TypeConstructor -> B.TypeConstructor
desugarTypeConstructor ctr =
    { id = qualify (Id.simple ctr.name)
    , arity = List.length ctr.args
    }


desugarStmt : F.Stmt -> Result DesugarError (List B.Decl)
desugarStmt stmt =
    case stmt of
        F.SLet r ->
            desugarLet r
                |> List.singleton
                |> Ok

        F.SLetBang r ->
            Debug.Extra.todo1 "desugar SLetBang" r

        F.SBang bang ->
            desugarBang bang

        F.SFunctionDef r ->
            desugarFunctionDef r
                |> List.singleton
                |> Ok

        F.SBinaryOperatorDef r ->
            Debug.Extra.todo1 "desugar SBinaryOperatorDef" r

        F.SUnaryOperatorDef r ->
            Debug.Extra.todo1 "desugar SUnaryOperatorDef" r

        F.SValueAnnotation r ->
            -- We're throwing away type annotations
            Ok []

        F.SBinaryOperatorAnnotation r ->
            -- We're throwing away type annotations
            Ok []

        F.SUnaryOperatorAnnotation r ->
            -- We're throwing away type annotations
            Ok []

        F.SUseModule r ->
            Debug.Extra.todo1 "desugar SUseModule" r


desugarFunctionDef :
    { mod : F.LetModifier
    , name : String
    , branches : NonemptyList F.FunctionBranch
    }
    -> B.Decl
desugarFunctionDef r =
    -- TODO do something about the mod
    let
        { args, body } =
            F.functionDefToSingleFunction r
    in
    B.DFunctionDef
        { id = qualify (Id.simple r.name)
        , args = List.map desugarPattern args
        , body = desugarExpr body
        }


desugarLet :
    { mod : F.LetModifier
    , lhs : F.Pattern
    , type_ : Maybe F.Type
    , expr : F.Expr
    }
    -> B.Decl
desugarLet letStmt =
    -- TODO mod
    -- TODO type
    B.DLetStmt
        { lhs = desugarPattern letStmt.lhs
        , expr = desugarExpr letStmt.expr
        }


desugarExpr : F.Expr -> B.Expr
desugarExpr e =
    case e of
        F.Int n ->
            B.Int n

        F.Float n ->
            B.Float n

        F.Char c ->
            B.Char c

        F.String s ->
            B.String s

        F.Bool b ->
            B.Bool b

        F.Unit ->
            B.Unit

        F.Tuple tuple ->
            B.Tuple (List.map desugarExpr tuple)

        F.List list ->
            B.List (List.map desugarExpr list)

        F.Record contents ->
            B.Record
                { sortedFields =
                    contents
                        |> List.concatMap
                            (\content ->
                                case content of
                                    F.Field f ->
                                        [ { field = f.field
                                          , expr = desugarExpr f.expr
                                          }
                                        ]

                                    F.Pun field ->
                                        Debug.Extra.todo1 "desugar record content - field" field

                                    F.Spread id ->
                                        Debug.Extra.todo1 "desugar record content - spread" id
                            )
                        |> List.sortBy .field
                }

        F.UnaryOp uop expr ->
            Debug.Extra.todo1 "desugar unary op" ( uop, expr )

        F.BinaryOp first bop second ->
            Debug.Extra.todo1 "desugar binary op" ( first, bop, second )

        F.Call { fn, args } ->
            case args of
                [] ->
                    desugarExpr fn

                first :: rest ->
                    -- TODO check if this order (foldl) doesn't introduce bugs
                    List.foldl
                        (\nextArg accFn ->
                            B.FnCall1
                                { fn = accFn
                                , arg = desugarExpr nextArg
                                }
                        )
                        (B.FnCall1
                            { fn = desugarExpr fn
                            , arg = desugarExpr first
                            }
                        )
                        rest

        F.RecordGet r ->
            B.FnCall1
                { fn = B.RecordGetter r.field
                , arg = desugarExpr r.record
                }

        F.Block r ->
            Debug.Extra.todo1 "desugar block" r

        F.EffectBlock r ->
            Debug.Extra.todo1 "desugar effect block" r

        F.Constructor_ { id, args } ->
            B.Constructor_
                { id = qualify id
                , args = List.map desugarExpr args
                }

        F.Identifier id ->
            B.RootIdentifier (qualify id)

        F.RootIdentifier id ->
            B.RootIdentifier (qualify id)

        F.OpIdentifier op ->
            Debug.todo "desugar op identifier" op

        F.Lambda { args, body } ->
            case List.reverse args of
                [] ->
                    Debug.todo "desugar lambda - 0-arg to 1-arg (using unit) B.Lambda1"

                last :: revButLast ->
                    -- TODO check if this order (reversing, foldl) doesn't introduce bugs
                    List.foldl
                        (\earlierArg accExpr ->
                            B.Lambda1
                                { arg = Debug.Extra.todo1 "desugar lambda - earlierArg - desugar from F.Pattern to String or change type of B.Lambda1.arg" earlierArg
                                , body = accExpr
                                }
                        )
                        (B.Lambda1
                            { arg = Debug.Extra.todo1 "last - desugar from F.Pattern to String or change type of B.Lambda1.arg" last
                            , body = desugarExpr body
                            }
                        )
                        revButLast

        F.RecordGetter getter ->
            B.RecordGetter getter

        F.If { cond, then_, else_ } ->
            B.If
                { cond = desugarExpr cond
                , then_ = desugarExpr then_
                , else_ = desugarExpr else_
                }

        F.Case r ->
            Debug.Extra.todo1 "desugar case..of" r


qualify : Id -> QualifiedId
qualify id =
    {- TODO take some more info as inputs

        - toplevel program
        - the current stack of modules that you're in
        - the current set of `use`'d modules in your scope

       and solve this in its most general form.

       (And then there's `import` which I'm totally ignoring for now...)

       TODO TODO TODO Until then, this is very very naive and wrong except for
       the very simplest programs.
    -}
    { qualifiers =
        NonemptyList.fromList id.qualifiers
            |> Maybe.withDefault (NonemptyList.singleton Env.rootModule.name)
    , name = id.name
    }


desugarPatternWithType : ( F.Pattern, Maybe F.Type ) -> B.Pattern
desugarPatternWithType ( pattern, _ ) =
    let
        _ =
            Debug.log "TODO desugarPatternWithType: do something with the type?" ()
    in
    desugarPattern pattern


desugarPattern : F.Pattern -> B.Pattern
desugarPattern p =
    case p of
        F.PUnit ->
            B.PUnit

        F.PVar name ->
            B.PVar name

        F.PConstructor { id, args } ->
            {- TODO if we're in a Let statement, check the constructor is a
               singleton one (type Foo = Bar(Int,Int)) and so it should allow
               the `Bar(a,b) = someFoo` destructuring.
            -}
            B.PConstructor
                { id = qualify id
                , args = List.map desugarPattern args
                }

        F.PInt int ->
            B.PInt int

        F.PFloat float ->
            B.PFloat float

        F.PChar char ->
            B.PChar char

        F.PString str ->
            B.PString str

        F.PBool bool ->
            B.PBool bool

        F.PList list ->
            B.PList (List.map desugarPattern list)

        F.PTuple tuple ->
            B.PTuple (List.map desugarPattern tuple)

        F.PWildcard ->
            B.PWildcard

        F.PSpread name ->
            B.PSpread name

        F.PRecordSpread ->
            B.PRecordSpread

        F.PRecordFields fields ->
            B.PRecordFields fields

        F.PAs alias_ pattern ->
            B.PAs alias_ (desugarPattern pattern)

        F.PUnaryOpDef r ->
            Debug.Extra.todo1 "uh... desugar PUnaryOpDef?" r

        F.PBinaryOpDef r ->
            Debug.Extra.todo1 "uh... desugar PBinaryOpDef?" r

        F.POr _ _ ->
            Debug.Extra.todo1 "desugar POr" p


desugarBang : F.Bang -> Result DesugarError (List B.Decl)
desugarBang bang =
    case bang of
        F.BValue expr ->
            Debug.Extra.todo1 "desugar bang value" expr

        F.BCall r ->
            Debug.Extra.todo1 "desugar bang call" r
