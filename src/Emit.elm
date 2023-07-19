module Emit exposing (emitProgram)

import AST exposing (..)
import HVM.AST as HVM exposing (Rule, Term(..))
import Id


emitProgram : AST.Program -> HVM.File
emitProgram program =
    { rules =
        mainRule program
            :: List.concatMap (declToRules "") program
    }


mainRule : AST.Program -> Rule
mainRule program =
    { lhs = Ctr { name = "Main", args = [] }
    , rhs = Str "TODO main rule"
    }


todoRule : String -> a -> List Rule
todoRule message thing =
    [ { lhs = todoTerm message
      , rhs = Str <| Debug.toString thing
      }
    ]


todoTerm : String -> Term
todoTerm message =
    Ctr { name = "Todo", args = [ Str message ] }


ctr : String -> List Term -> Term
ctr name args =
    Ctr { name = name, args = args }


tuple : List Term -> Term
tuple terms =
    let
        len =
            List.length terms
    in
    ctr
        ("Data.Cara.tuple" ++ String.fromInt len)
        terms


declToRules : String -> Decl -> List Rule
declToRules modulePath decl =
    case decl of
        DTypeAlias _ ->
            []

        DType r ->
            -- We'll just create the constructors as needed.
            -- All we need is that they're fully qualified.
            []

        DModule m ->
            moduleToRules modulePath m

        DExtendModule r ->
            todoRule "DExtendModule" r

        DStatement stmt ->
            stmtToRules modulePath stmt

        DUnitTest r ->
            todoRule "DUnitTest" r

        DParameterizedTest r ->
            todoRule "DParameterizedTest" r

        DPropertyTypeTest r ->
            todoRule "DPropertyTypeTest" r

        DPropertyGenTest r ->
            todoRule "DPropertyGenTest" r


moduleToRules :
    String
    ->
        { mod : ModuleModifier
        , name : String
        , decls : List Decl
        }
    -> List Rule
moduleToRules modulePath newModule =
    -- TODO mod
    newModule.decls
        |> List.concatMap (declToRules (addModule newModule.name modulePath))


qualified : String -> String -> String
qualified modules var =
    -- Hacky?
    modules
        |> addModule var


addModule : String -> String -> String
addModule newModule oldModules =
    if String.isEmpty oldModules then
        newModule

    else
        [ oldModules, newModule ]
            |> String.join "."


stmtToRules : String -> Stmt -> List Rule
stmtToRules modulePath stmt =
    case stmt of
        SLet r ->
            todoRule "SLet" r

        SLetBang r ->
            todoRule "SLetBang" r

        SBang r ->
            todoRule "SBang" r

        SFunctionDef r ->
            [ functionDefToRule modulePath r ]

        SBinaryOperatorDef r ->
            todoRule "SBinaryOperatorDef" r

        SUnaryOperatorDef r ->
            todoRule "SUnaryOperatorDef" r

        SValueAnnotation r ->
            todoRule "SValueAnnotation" r

        SBinaryOperatorAnnotation r ->
            todoRule "SBinaryOperatorAnnotation" r

        SUnaryOperatorAnnotation r ->
            todoRule "SUnaryOperatorAnnotation" r

        SUseModule r ->
            todoRule "SUseModule" r


functionDefToRule :
    String
    ->
        { name : String
        , args : List Pattern
        , body : Expr
        }
    -> Rule
functionDefToRule modulePath ({ name, args, body } as r) =
    let
        _ =
            Debug.log "function def to rule" r
    in
    { lhs =
        functionTerm
            (Var (qualified modulePath name))
            (List.map patternToTerm args)
    , rhs = exprToTerm body
    }


functionTerm : Term -> List Term -> Term
functionTerm function args =
    let
        oneArgApplication : Term -> Term -> Term
        oneArgApplication term fn =
            App
                { function = fn
                , arg = term
                }
    in
    List.foldl oneArgApplication function args


exprToTerm : Expr -> Term
exprToTerm expr =
    case expr of
        Int n ->
            U60 n

        Float f ->
            F60 f

        Char str ->
            ctr "Data.Cara.char" [ Str str ]

        String str ->
            Str str

        Bool bool ->
            if bool then
                U60 0

            else
                U60 1

        Unit ->
            ctr "Data.Cara.unit" []

        Tuple xs ->
            tuple (List.map exprToTerm xs)

        List xs ->
            Lst (List.map exprToTerm xs)

        Record contents ->
            todoTerm "exprToTerm record"

        UnaryOp op e ->
            todoTerm "exprToTerm unaryOp"

        BinaryOp left op right ->
            todoTerm "exprToTerm binaryOp"

        Call { fn, args } ->
            functionTerm
                (exprToTerm fn)
                (List.map exprToTerm args)

        RecordGet { record, field } ->
            todoTerm "exprToTerm recordGet"

        Block { stmts, ret } ->
            todoTerm "exprToTerm block"

        EffectBlock { monadModule, stmts, ret } ->
            todoTerm "exprToTerm effect block"

        Constructor_ { id, args } ->
            ctr
                (Id.toString id)
                (List.map exprToTerm args)

        Identifier id ->
            Var (Id.toString id)

        RootIdentifier id ->
            todoTerm "exprToTerm rootIdentifier"

        Lambda { args, body } ->
            todoTerm "exprToTerm lambda"

        RecordGetter getter ->
            todoTerm "exprToTerm record getter"

        If { cond, then_, else_ } ->
            ctr "Data.U60.if"
                (List.map exprToTerm
                    [ cond
                    , then_
                    , else_
                    ]
                )

        Case { subject, branches } ->
            todoTerm "exprToTerm case"


patternToTerm : Pattern -> Term
patternToTerm pattern =
    case pattern of
        PUnit ->
            ctr "Data.Cara.unit" []

        PVar var ->
            Var var

        PConstructor { id, args } ->
            ctr
                (Id.toString id)
                (List.map patternToTerm args)

        PInt int ->
            U60 int

        PFloat float ->
            F60 float

        PList ps ->
            Lst (List.map patternToTerm ps)

        PTuple ps ->
            tuple (List.map patternToTerm ps)

        PWildcard ->
            Var "_"

        PSpread maybeVar ->
            Debug.todo "pattern to term - spread"

        PRecordSpread ->
            Debug.todo "pattern to term - record spread"

        PRecordFields recordFields ->
            Debug.todo "pattern to term - record fields"

        -- Shouldn't happen:
        PUnaryOpDef _ ->
            Debug.todo "pattern to term - unary op def"

        PBinaryOpDef _ ->
            Debug.todo "pattern to term - binary op def"
