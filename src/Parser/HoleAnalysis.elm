module Parser.HoleAnalysis exposing
    ( HoleAnalysis(..)
    , analyzeHoles
    )

import AST.Frontend as AST
    exposing
        ( Bang(..)
        , BangOrExpr(..)
        , Expr(..)
        , RecordExprContent(..)
        , Stmt(..)
        )
import Error exposing (ParserError)
import NonemptyList


type HoleAnalysis
    = NoHoles
    | OnlyUnderscore
    | OnlyNumbered { max : Int }
    | Error ParserError


analyzeHoles : Expr -> HoleAnalysis
analyzeHoles expr =
    case expr of
        Identifier { qualifiers, name } ->
            if List.isEmpty qualifiers && String.startsWith "_" name then
                if name == "_" then
                    OnlyUnderscore

                else
                    case String.toInt (String.dropLeft 1 name) of
                        Nothing ->
                            Error <| Error.NonNumberedHole name

                        Just n ->
                            OnlyNumbered { max = n }

            else
                NoHoles

        Constructor_ r ->
            many r.args

        Lambda r ->
            analyzeHoles r.body

        UnaryOp _ arg ->
            analyzeHoles arg

        BinaryOp left _ right ->
            many [ left, right ]

        RecordGet r ->
            analyzeHoles r.record

        Tuple xs ->
            many xs

        List xs ->
            many xs

        Record contents ->
            List.map analyzeRecordExprContent contents
                |> mergeMany

        Call r ->
            many (r.fn :: r.args)

        If r ->
            many [ r.cond, r.then_, r.else_ ]

        Case r ->
            many (r.subject :: List.map .body r.branches)

        Block r ->
            mergeMany (analyzeHoles r.ret :: List.map analyzeStmt r.stmts)

        EffectBlock r ->
            mergeMany (analyzeBangOrExpr r.ret :: List.map analyzeStmt r.stmts)

        Int _ ->
            NoHoles

        Float _ ->
            NoHoles

        Char _ ->
            NoHoles

        String _ ->
            NoHoles

        Bool _ ->
            NoHoles

        Unit ->
            NoHoles

        RootIdentifier _ ->
            NoHoles

        OpIdentifier _ ->
            NoHoles

        RecordGetter _ ->
            NoHoles


analyzeBangOrExpr : BangOrExpr -> HoleAnalysis
analyzeBangOrExpr boe =
    case boe of
        B bang ->
            analyzeBang bang

        E expr ->
            analyzeHoles expr


analyzeRecordExprContent : RecordExprContent -> HoleAnalysis
analyzeRecordExprContent content =
    case content of
        Field r ->
            analyzeHoles r.expr

        Pun _ ->
            NoHoles

        Spread _ ->
            NoHoles


analyzeStmt : Stmt -> HoleAnalysis
analyzeStmt stmt =
    case stmt of
        SLet r ->
            analyzeHoles r.expr

        SLetBang r ->
            analyzeBang r.bang

        SBang bang ->
            analyzeBang bang

        SFunctionDef r ->
            mergeMany
                (r.branches
                    |> NonemptyList.toList
                    |> List.map (.body >> analyzeHoles)
                )

        SBinaryOperatorDef r ->
            analyzeHoles r.body

        SUnaryOperatorDef r ->
            analyzeHoles r.body

        SValueAnnotation r ->
            NoHoles

        SBinaryOperatorAnnotation r ->
            NoHoles

        SUnaryOperatorAnnotation r ->
            NoHoles

        SUseModule _ ->
            NoHoles


analyzeBang : Bang -> HoleAnalysis
analyzeBang bang =
    case bang of
        BValue expr ->
            analyzeHoles expr

        BCall r ->
            many (r.fn :: r.args)


maybe : (a -> HoleAnalysis) -> Maybe a -> HoleAnalysis
maybe fn val =
    val
        |> Maybe.map fn
        |> Maybe.withDefault NoHoles


many : List Expr -> HoleAnalysis
many xs =
    xs
        |> List.map analyzeHoles
        |> mergeMany


merge : HoleAnalysis -> HoleAnalysis -> HoleAnalysis
merge a b =
    case ( a, b ) of
        ( NoHoles, _ ) ->
            b

        ( _, NoHoles ) ->
            a

        ( Error _, _ ) ->
            a

        ( _, Error _ ) ->
            b

        ( OnlyUnderscore, OnlyNumbered _ ) ->
            Error Error.MixedHoles

        ( OnlyNumbered _, OnlyUnderscore ) ->
            Error Error.MixedHoles

        ( OnlyUnderscore, OnlyUnderscore ) ->
            OnlyUnderscore

        ( OnlyNumbered a_, OnlyNumbered b_ ) ->
            OnlyNumbered { max = max a_.max b_.max }


mergeMany : List HoleAnalysis -> HoleAnalysis
mergeMany xs =
    List.foldl merge NoHoles xs
