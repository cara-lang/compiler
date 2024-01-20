module Desugar exposing (desugarProgram)

import AST.Backend as B
import AST.Frontend as F
import Error exposing (DesugarError)
import Id
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
            Debug.todo "desugar type alias"

        F.DType r ->
            Debug.todo "desugar type"

        F.DModule r ->
            Debug.todo "desugar module"

        F.DExtendModule r ->
            Debug.todo "desugar extend module"

        F.DStatement stmt ->
            desugarStmt stmt

        F.DUnitTest r ->
            Debug.todo "desugar unit test"

        F.DParameterizedTest r ->
            Debug.todo "desugar parameterized test"

        F.DPropertyTypeTest r ->
            Debug.todo "desugar property type test"

        F.DPropertyGenTest r ->
            Debug.todo "desugar property gen test"


desugarStmt : F.Stmt -> Result DesugarError (List B.Decl)
desugarStmt stmt =
    case stmt of
        F.SLet r ->
            desugarLet r
                |> Result.map List.singleton

        F.SLetBang r ->
            Debug.todo "desugar sletbang"

        F.SBang bang ->
            desugarBang bang

        F.SFunctionDef r ->
            Debug.todo "desugar sfunctiondef"

        F.SBinaryOperatorDef r ->
            Debug.todo "desugar sbinaryoperatordef"

        F.SUnaryOperatorDef r ->
            Debug.todo "desugar sunaryoperatordef"

        F.SValueAnnotation r ->
            Debug.todo "desugar svalueannotation"

        F.SBinaryOperatorAnnotation r ->
            Debug.todo "desugar sbinaryoperatorannotation"

        F.SUnaryOperatorAnnotation r ->
            Debug.todo "desugar sunaryoperatorannotation"

        F.SUseModule r ->
            Debug.todo "desugar susemodule"


desugarLet :
    { mod : F.LetModifier
    , lhs : F.Pattern
    , type_ : Maybe F.Type
    , expr : F.Expr
    }
    -> Result DesugarError B.Decl
desugarLet letStmt =
    -- TODO mod
    -- TODO type
    B.DLetStmt
        { lhs = desugarPattern letStmt.lhs
        , expr = desugarExpr letStmt.expr
        }
        |> Ok


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
            Debug.todo "desugar record"

        F.UnaryOp uop expr ->
            Debug.todo "desugar unary op"

        F.BinaryOp first bop second ->
            Debug.todo "desugar binary op"

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

        F.RecordGet { record, field } ->
            Debug.todo "desugar record get"

        F.Block { stmts, ret } ->
            Debug.todo "desugar block"

        F.EffectBlock { monadModule, stmts, ret } ->
            Debug.todo "desugar effect block"

        F.Constructor_ { id, args } ->
            B.Constructor_
                { id = Debug.todo <| "desugar constructor: qualify `id`: " ++ Id.toString id
                , args = List.map desugarExpr args
                }

        F.Identifier id ->
            Debug.todo "desugar identifier to root identifier"

        F.RootIdentifier id ->
            B.RootIdentifier (Debug.todo "desugar root identifier: qualify `id`")

        F.Lambda { args, body } ->
            case List.reverse args of
                [] ->
                    Debug.todo "desugar 0-arg to 1-arg (using unit) B.Lambda1"

                last :: revButLast ->
                    -- TODO check if this order (reversing, foldl) doesn't introduce bugs
                    List.foldl
                        (\earlierArg accExpr ->
                            B.Lambda1
                                { arg = Debug.todo "earlierArg - desugar from F.Pattern to String or change type of B.Lambda1.arg"
                                , body = accExpr
                                }
                        )
                        (B.Lambda1
                            { arg = Debug.todo "last - desugar from F.Pattern to String or change type of B.Lambda1.arg"
                            , body = desugarExpr body
                            }
                        )
                        revButLast

        F.RecordGetter getter ->
            Debug.todo "desugar record getter"

        F.If { cond, then_, else_ } ->
            B.If
                { cond = desugarExpr cond
                , then_ = desugarExpr then_
                , else_ = desugarExpr else_
                }

        F.Case { subject, branches } ->
            Debug.todo "desugar case..of"


desugarPattern : F.Pattern -> B.Pattern
desugarPattern p =
    case p of
        F.PUnit ->
            B.PUnit

        F.PVar name ->
            B.PVar name

        F.PConstructor { id, args } ->
            B.PConstructor
                { id = Debug.todo "desugarPattern PConstructor - qualify the `id`"
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

        F.PUnaryOpDef _ ->
            Debug.todo "uh... desugar PUnaryOpDef"

        F.PBinaryOpDef _ ->
            Debug.todo "uh... desugar PBinaryOpDef"


desugarBang : F.Bang -> Result DesugarError (List B.Decl)
desugarBang bang =
    case bang of
        F.BValue expr ->
            Debug.todo "desugar bang value"

        F.BCall { fn, args } ->
            Debug.todo "desugar bang call"
