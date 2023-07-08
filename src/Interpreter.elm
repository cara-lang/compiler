module Interpreter exposing (interpretProgram)

import AST exposing (..)
import Bitwise
import Dict exposing (Dict)
import Effect
import Env exposing (Env)
import EnvDict exposing (EnvDict)
import Error exposing (InterpreterError(..))
import Id exposing (Id)
import Interpreter.Internal as Interpreter exposing (Interpreter)
import Interpreter.Outcome as Outcome exposing (Outcome(..))
import Intrinsic exposing (Intrinsic(..))
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)
import Tree.Zipper as Zipper
import Value exposing (Value(..))


interpretProgram : Interpreter AST.Program ()
interpretProgram =
    Interpreter.traverse interpretDecl
        |> Interpreter.map (\_ -> ())


interpretDecl : Interpreter Decl ()
interpretDecl =
    \env decl ->
        case decl of
            DModule r ->
                interpretModule env r

            DStatement stmt ->
                interpretStatement env stmt

            DType r ->
                interpretTypeDecl env r

            DTypeAlias r ->
                interpretTypeAlias env r

            DUnitTest r ->
                interpretUnitTest env r

            DParameterizedTest r ->
                interpretParameterizedTest env r

            DPropertyTypeTest r ->
                interpretPropertyTypeTest env r

            DPropertyGenTest r ->
                interpretPropertyGenTest env r

            _ ->
                Debug.todo <| "Unimplemented interpretDecl: " ++ Debug.toString decl


interpretUnitTest : Interpreter { name : Maybe String, expr : Expr } ()
interpretUnitTest =
    \env { name, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretUnitTest" ( name, expr )
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretParameterizedTest : Interpreter { name : Maybe String, table : List Expr, args : List Pattern, expr : Expr } ()
interpretParameterizedTest =
    \env { name, table, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretParameterizedTest" ( name, table, ( args, expr ) )
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretPropertyTypeTest : Interpreter { name : Maybe String, types : Type, args : List Pattern, expr : Expr } ()
interpretPropertyTypeTest =
    \env { name, types, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretPropertyTypeTest" ( name, types, ( args, expr ) )
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretPropertyGenTest : Interpreter { name : Maybe String, gens : Expr, args : List Pattern, expr : Expr } ()
interpretPropertyGenTest =
    \env { name, gens, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretPropertyGenTest" ( name, gens, ( args, expr ) )
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretStatement : Interpreter Stmt ()
interpretStatement =
    \env stmt ->
        case stmt of
            SLet r ->
                interpretLet env r

            SLetBang r ->
                interpretLetBang env r

            SBang bang ->
                interpretBang env bang
                    -- Throw the result away
                    |> Outcome.map (\_ -> ())

            SFunction r ->
                interpretFunction env r

            SBinaryOperator r ->
                interpretBinaryOp env r

            SUnaryOperator r ->
                interpretUnaryOp env r

            SValueAnnotation r ->
                interpretValueAnnotation env r

            SBinaryOperatorAnnotation r ->
                interpretBinaryOpAnnotation env r

            SUnaryOperatorAnnotation r ->
                interpretUnaryOpAnnotation env r

            SUseModule r ->
                interpretUseModule env r


interpretFunction : Interpreter { name : String, args : List Pattern, body : Expr } ()
interpretFunction =
    \env { name, args, body } ->
        -- TODO somehow group related function declarations together and make one case..of from them (also get the modifier from the annotation)
        let
            envWithFn : Env Value
            envWithFn =
                env
                    |> Env.add name
                        (VClosure
                            { args = args
                            , body = body
                            , env = env
                            }
                        )
        in
        Outcome.succeed envWithFn ()


interpretBinaryOp : Interpreter { op : BinaryOp, left : Pattern, right : Pattern, body : Expr } ()
interpretBinaryOp =
    \env { op, left, right, body } ->
        let
            _ =
                Debug.log "TODO do something in interpretBinaryOp" ( op, ( left, right, body ) )
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUnaryOp : Interpreter { op : UnaryOp, arg : Pattern, body : Expr } ()
interpretUnaryOp =
    \env { op, arg, body } ->
        let
            _ =
                Debug.log "TODO do something in interpretUnaryOp" ( op, arg, body )
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretValueAnnotation : Interpreter { mod : LetModifier, name : String, type_ : Type } ()
interpretValueAnnotation =
    \env { mod, name, type_ } ->
        let
            _ =
                Debug.log "TODO do something in interpretValueAnnotation" ( mod, name, type_ )
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretBinaryOpAnnotation : Interpreter { mod : LetModifier, op : BinaryOp, left : Type, right : Type, ret : Type } ()
interpretBinaryOpAnnotation =
    \env { mod, op, left, right, ret } ->
        let
            _ =
                Debug.log "TODO do something in interpretBinaryOpAnnotation" ( mod, op, ( left, right, ret ) )
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUnaryOpAnnotation : Interpreter { mod : LetModifier, op : UnaryOp, arg : Type, ret : Type } ()
interpretUnaryOpAnnotation =
    \env { mod, op, arg, ret } ->
        let
            _ =
                Debug.log "TODO do something in interpretUnaryOpAnnotation" ( mod, op, ( arg, ret ) )
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUseModule : Interpreter Id ()
interpretUseModule =
    \env moduleId ->
        let
            _ =
                Debug.log "TODO do something in interpretUseModule" moduleId
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretTypeDecl :
    Interpreter
        { mod : TypeModifier
        , name : String
        , vars : List String
        , constructors : List Constructor
        }
        ()
interpretTypeDecl =
    \env r ->
        -- TODO interpret the modifier
        let
            additions : EnvDict Value
            additions =
                r.constructors
                    |> List.map
                        (\c ->
                            let
                                names : List String
                                names =
                                    List.indexedMap
                                        (\i _ -> "arg" ++ String.fromInt i)
                                        c.args
                            in
                            ( Id.local c.name
                            , VClosure
                                { args = List.map PVar names
                                , body =
                                    Constructor_
                                        { id =
                                            -- TODO what about the module?
                                            Id.local r.name
                                        , args = List.map (Identifier << Id.local) names
                                        }
                                , env = env
                                }
                            )
                        )
                    |> EnvDict.fromList

            newEnv : Env Value
            newEnv =
                Env.addDict additions env
        in
        Outcome.succeed newEnv ()


interpretTypeAlias :
    Interpreter
        { mod : TypeAliasModifier
        , name : String
        , vars : List String
        , body : Type
        }
        ()
interpretTypeAlias =
    \env { mod, name, vars, body } ->
        let
            _ =
                Debug.log "TODO do something in interpretTypeAlias" ( mod, name, ( vars, body ) )
        in
        -- TODO make type aliases do something
        Outcome.succeed env ()


interpretBang : Interpreter Bang Value
interpretBang =
    \env bang ->
        case bang of
            BValue expr ->
                interpretBangValue env expr

            BCall r ->
                interpretBangCall env r


interpretBangValue : Interpreter Expr Value
interpretBangValue =
    \env expr ->
        Interpreter.do (interpretExpr env expr) <|
            \env1 val ->
                Debug.todo <| "what the heck are we getting here? " ++ Debug.toString val


interpretBangCall : Interpreter { fn : Expr, args : List Expr } Value
interpretBangCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <|
            \env1 fnVal ->
                case fnVal of
                    VIntrinsic intrinsic ->
                        interpretIntrinsicCall env1 ( intrinsic, args )

                    _ ->
                        Debug.todo "Unsupported Value node in the `fn` position of a BangCall"


interpretIntrinsicCall : Interpreter ( Intrinsic, List Expr ) Value
interpretIntrinsicCall =
    \env ( intrinsicFn, args ) ->
        case intrinsicFn of
            IoPrintln ->
                case args of
                    [ arg ] ->
                        interpretExpr env arg
                            |> Interpreter.andThen interpretPrintln
                            |> Outcome.map (\() -> VUnit)

                    _ ->
                        Outcome.fail UnexpectedArity


interpretLet :
    Interpreter
        { mod : LetModifier
        , lhs : Pattern
        , type_ : Maybe Type
        , expr : Expr
        }
        ()
interpretLet =
    \env { lhs, expr } ->
        -- TODO interpret the modifier
        -- TODO interpret the type
        interpretExpr env expr
            |> Outcome.map (\value -> ( lhs, value ))
            |> Interpreter.andThen interpretPattern
            |> Outcome.mapBoth
                (\env_ envAdditions ->
                    case envAdditions of
                        Nothing ->
                            Debug.todo "Pattern didn't match the expr. TODO Report this as user error?"

                        Just additions ->
                            ( env_ |> Env.addDict additions
                            , ()
                            )
                )


interpretLetBang :
    Interpreter
        { mod : LetModifier
        , lhs : Pattern
        , type_ : Maybe Type
        , bang : Bang
        }
        ()
interpretLetBang =
    \env { lhs, bang } ->
        -- TODO interpret the mod
        -- TODO interpret the type_
        interpretBang env bang
            |> Outcome.map (\value -> ( lhs, value ))
            |> Interpreter.andThen interpretPattern
            |> Outcome.mapBoth
                (\env_ envAdditions ->
                    case envAdditions of
                        Nothing ->
                            Debug.todo "Pattern didn't match the expr. TODO Report this as user error?"

                        Just additions ->
                            ( env_ |> Env.addDict additions
                            , ()
                            )
                )


{-| Returns (NEW) env additions, instead of the whole env.
Nothing is returned if the pattern doesn't match the expr.
-}
interpretPattern : Interpreter ( Pattern, Value ) (Maybe (EnvDict Value))
interpretPattern =
    \env ( pattern, value ) ->
        case pattern of
            PVar var ->
                EnvDict.singleton (Id.local var) value
                    |> Just
                    |> Outcome.succeed env

            PInt int ->
                Outcome.succeed env <|
                    case value of
                        VInt n ->
                            if int == n then
                                Just EnvDict.empty

                            else
                                Nothing

                        _ ->
                            Nothing

            PRecordSpread ->
                case value of
                    VRecord fields ->
                        fields
                            |> Dict.toList
                            |> List.map (Tuple.mapFirst Id.local)
                            |> EnvDict.fromList
                            |> Just
                            |> Outcome.succeed env

                    -- TODO tuple record spread
                    _ ->
                        Debug.todo <| "interpret pattern {..} - other: " ++ Debug.toString value

            PRecordFields wantedFields ->
                let
                    wantedFieldsSet =
                        Set.fromList wantedFields
                in
                case value of
                    VRecord fields ->
                        fields
                            |> Dict.toList
                            |> List.filter (\( field, _ ) -> Set.member field wantedFieldsSet)
                            |> List.map (Tuple.mapFirst Id.local)
                            |> EnvDict.fromList
                            |> Just
                            |> Outcome.succeed env

                    _ ->
                        Outcome.succeed env Nothing

            PConstructor { id, args } ->
                case value of
                    VConstructor r ->
                        if r.id == id then
                            let
                                pairs : List ( Pattern, Value )
                                pairs =
                                    List.map2 Tuple.pair args r.args
                            in
                            Interpreter.do (Interpreter.traverse interpretPattern env pairs) <|
                                \env1 maybeAdditions ->
                                    case Maybe.combine maybeAdditions of
                                        Nothing ->
                                            Outcome.succeed env Nothing

                                        Just additions ->
                                            Outcome.succeed env (Just (EnvDict.unionAll additions))

                        else
                            Outcome.succeed env Nothing

                    _ ->
                        Outcome.succeed env Nothing

            _ ->
                Debug.todo <| "interpret pattern - other: " ++ Debug.toString pattern


interpretExpr : Interpreter Expr Value
interpretExpr =
    \env expr ->
        case expr of
            Int n ->
                Outcome.succeed env (VInt n)

            Float n ->
                Outcome.succeed env (VFloat n)

            String s ->
                Outcome.succeed env (VString s)

            Identifier id ->
                interpretIdentifier env id

            RootIdentifier id ->
                interpretRootIdentifier env id

            Constructor_ r ->
                interpretConstructor env r

            Bool bool ->
                Outcome.succeed env (VBool bool)

            Unit ->
                Outcome.succeed env VUnit

            If r ->
                interpretIf env r

            Case r ->
                interpretCase env r

            List xs ->
                interpretList env xs

            Tuple xs ->
                interpretTuple env xs

            Record fields ->
                interpretRecord env fields

            UnaryOp op e ->
                interpretUnaryOpCall env ( op, e )

            BinaryOp left op right ->
                interpretBinaryOpCall env ( left, op, right )

            RecordGet r ->
                interpretRecordGet env r

            RecordGetter field ->
                Outcome.succeed env (VRecordGetter field)

            Call r ->
                interpretCall env r

            Lambda r ->
                interpretLambda env r

            Block r ->
                interpretBlock env r

            EffectBlock r ->
                interpretEffectBlock env r

            _ ->
                Debug.todo <| "Unimplemented interpretExpr: " ++ Debug.toString expr


interpretCall : Interpreter { fn : Expr, args : List Expr } Value
interpretCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <|
            \env1 fnVal ->
                Interpreter.do (Interpreter.traverse interpretExpr env1 args) <|
                    \env2 argVals ->
                        case ( fnVal, argVals ) of
                            ( VRecordGetter field, [ VTuple values ] ) ->
                                interpretRecordGetTuple env2 ( values, field )

                            ( VClosure r, _ ) ->
                                case compare (List.length r.args) (List.length argVals) of
                                    LT ->
                                        Debug.todo "call closure #args < #values"

                                    GT ->
                                        Debug.todo "call closure #args > #values"

                                    EQ ->
                                        let
                                            pairs : List ( Pattern, Value )
                                            pairs =
                                                List.map2 Tuple.pair r.args argVals
                                        in
                                        Interpreter.do (Interpreter.traverse interpretPattern env2 pairs) <|
                                            \env3 maybeDicts ->
                                                case Maybe.combine maybeDicts of
                                                    Nothing ->
                                                        Outcome.fail PatternMismatch

                                                    Just dicts ->
                                                        Interpreter.do (interpretExpr (List.foldl Env.addDict r.env dicts) r.body) <|
                                                            \_ callResult ->
                                                                Outcome.succeed env3 callResult

                            _ ->
                                Debug.todo <| "interpretCall interpreted: " ++ Debug.toString ( fnVal, argVals )


interpretLambda : Interpreter { args : List Pattern, body : Expr } Value
interpretLambda =
    \env { args, body } ->
        Outcome.succeed env <| VClosure { args = args, body = body, env = env }


interpretBlock : Interpreter { stmts : List Stmt, ret : Expr } Value
interpretBlock =
    \env { stmts, ret } ->
        Interpreter.do (Interpreter.traverse interpretStatement env stmts) <|
            \env1 _ ->
                interpretExpr env1 ret


interpretEffectBlock : Interpreter { monadModule : Id, stmts : List Stmt, ret : BangOrExpr } Value
interpretEffectBlock =
    \env { monadModule, stmts, ret } ->
        -- TODO use the monad module
        Interpreter.do (Interpreter.traverse interpretStatement env stmts) <|
            \env1 _ ->
                interpretBangOrExpr env1 ret


interpretBangOrExpr : Interpreter BangOrExpr Value
interpretBangOrExpr =
    \env boe ->
        case boe of
            B bang ->
                interpretBang env bang

            E expr ->
                interpretExpr env expr


interpretUnaryOpCall : Interpreter ( UnaryOp, Expr ) Value
interpretUnaryOpCall =
    \env ( op, expr ) ->
        Interpreter.do (interpretExpr env expr) <|
            \env1 val ->
                interpretUnaryOpCallVal env1 ( op, val )


interpretUnaryOpCallVal : Interpreter ( UnaryOp, Value ) Value
interpretUnaryOpCallVal =
    \env ( op, val ) ->
        case ( op, val ) of
            ( NegateNum, VInt n ) ->
                Outcome.succeed env <| VInt (negate n)

            ( NegateNum, VFloat n ) ->
                Outcome.succeed env <| VFloat (negate n)

            ( NegateBool, VBool b ) ->
                Outcome.succeed env <| VBool (not b)

            ( NegateBin, VInt n ) ->
                Outcome.succeed env <| VInt (Bitwise.complement n)

            _ ->
                Debug.todo <| "Unimplemented interpretUnaryOpVal: " ++ Debug.toString ( op, val )


interpretBinaryOpCall : Interpreter ( Expr, BinaryOp, Expr ) Value
interpretBinaryOpCall =
    \env ( left, op, right ) ->
        Interpreter.do (interpretExpr env left) <|
            \env1 leftVal ->
                Interpreter.do (interpretExpr env1 right) <|
                    \env2 rightVal ->
                        interpretBinaryOpCallVal env2 ( leftVal, op, rightVal )


interpretBinaryOpCallVal : Interpreter ( Value, BinaryOp, Value ) Value
interpretBinaryOpCallVal =
    \env ( left, op, right ) ->
        case ( left, op, right ) of
            ( VInt a, Plus, VInt b ) ->
                Outcome.succeed env <| VInt (a + b)

            ( VInt a, Minus, VInt b ) ->
                Outcome.succeed env <| VInt (a - b)

            ( VInt a, Times, VInt b ) ->
                Outcome.succeed env <| VInt (a * b)

            ( VInt a, Div, VInt b ) ->
                Outcome.succeed env <| VInt (a // b)

            ( VInt a, Pow, VInt b ) ->
                Outcome.succeed env <| VInt (a ^ b)

            ( VInt a, Lte, VInt b ) ->
                Outcome.succeed env <| VBool (a <= b)

            ( VInt a, Lt, VInt b ) ->
                Outcome.succeed env <| VBool (a < b)

            ( VInt a, Eq, VInt b ) ->
                Outcome.succeed env <| VBool (a == b)

            ( VInt a, Neq, VInt b ) ->
                Outcome.succeed env <| VBool (a /= b)

            ( VInt a, Gt, VInt b ) ->
                Outcome.succeed env <| VBool (a > b)

            ( VInt a, Gte, VInt b ) ->
                Outcome.succeed env <| VBool (a >= b)

            ( VInt a, Mod, VInt b ) ->
                Outcome.succeed env <| VInt (a |> modBy b)

            ( VBool a, OrBool, VBool b ) ->
                Outcome.succeed env <| VBool (a || b)

            ( VBool a, AndBool, VBool b ) ->
                Outcome.succeed env <| VBool (a && b)

            ( VList a, Append, VList b ) ->
                Outcome.succeed env <| VList (a ++ b)

            ( a, Append, VList b ) ->
                Outcome.succeed env <| VList (a :: b)

            ( VList a, Append, b ) ->
                Outcome.succeed env <| VList (a ++ [ b ])

            ( VInt a, RangeInclusive, VInt b ) ->
                Outcome.succeed env <| VList (List.range a b |> List.map VInt)

            ( VInt a, RangeExclusive, VInt b ) ->
                Outcome.succeed env <| VList (List.range a (b - 1) |> List.map VInt)

            ( VInt a, OrBin, VInt b ) ->
                Outcome.succeed env <| VInt (Bitwise.or a b)

            ( VInt a, AndBin, VInt b ) ->
                Outcome.succeed env <| VInt (Bitwise.and a b)

            ( VInt a, XorBin, VInt b ) ->
                Outcome.succeed env <| VInt (Bitwise.xor a b)

            ( VInt a, ShiftL, VInt b ) ->
                Outcome.succeed env <| VInt (a |> Bitwise.shiftLeftBy b)

            ( VInt a, ShiftR, VInt b ) ->
                Outcome.succeed env <| VInt (a |> Bitwise.shiftRightBy b)

            ( VInt a, ShiftRU, VInt b ) ->
                Outcome.succeed env <| VInt (a |> Bitwise.shiftRightZfBy b)

            _ ->
                Debug.todo <| "Unimplemented interpretBinaryOp: " ++ Debug.toString ( left, op, right )


interpretRecordGet : Interpreter { record : Expr, field : String } Value
interpretRecordGet =
    \env { record, field } ->
        Interpreter.do (interpretExpr env record) <|
            \env1 recordVal ->
                case recordVal of
                    VTuple values ->
                        interpretRecordGetTuple env ( values, field )

                    _ ->
                        Debug.todo <| "Unimplemented interpretRecordGet: " ++ Debug.toString recordVal


specialTupleGetters : Dict String Int
specialTupleGetters =
    Dict.fromList
        [ ( "first", 0 )
        , ( "second", 1 )
        , ( "third", 2 )
        , ( "fourth", 3 )
        , ( "fifth", 4 )
        , ( "sixth", 5 )
        , ( "seventh", 6 )
        , ( "eighth", 7 )
        , ( "ninth", 8 )
        , ( "tenth", 9 )
        ]


interpretRecordGetTuple : Interpreter ( List Value, String ) Value
interpretRecordGetTuple =
    \env ( values, field ) ->
        let
            accessIndex : Int -> Outcome Value
            accessIndex index =
                case List.getAt index values of
                    Just value ->
                        Outcome.succeed env value

                    Nothing ->
                        Outcome.fail
                            (TupleLengthMismatch
                                { wanted = index
                                , length = List.length values
                                }
                            )

            unknownField : () -> Outcome Value
            unknownField () =
                Outcome.fail (TupleUnknownField field)
        in
        case Dict.get field specialTupleGetters of
            Just index ->
                accessIndex index

            Nothing ->
                if String.startsWith "el" field then
                    case String.toInt (String.dropLeft 2 field) of
                        Nothing ->
                            unknownField ()

                        Just index ->
                            accessIndex index

                else
                    unknownField ()


{-| Try to find the ID in the current Env, then in the parent, ... up to the root.
-}
interpretIdentifier : Interpreter Id Value
interpretIdentifier =
    \env id ->
        let
            go env_ =
                case env_ of
                    Nothing ->
                        Outcome.fail <| VarNotFound id

                    Just env__ ->
                        case Env.get id env__ of
                            Nothing ->
                                go (Zipper.parent env__)

                            Just value ->
                                Outcome.succeed env value
        in
        go (Just env)


{-| Try to find the ID in the root Env.
-}
interpretRootIdentifier : Interpreter Id Value
interpretRootIdentifier =
    \env id ->
        case Env.get id (Zipper.root env) of
            Nothing ->
                Outcome.fail <| RootVarNotFound id

            Just value ->
                Outcome.succeed env value


interpretConstructor : Interpreter { id : Id, args : List Expr } Value
interpretConstructor =
    \env { id, args } ->
        Interpreter.traverse interpretExpr env args
            |> Outcome.map (\argValues -> VConstructor { id = id, args = argValues })


interpretIf : Interpreter { cond : Expr, then_ : Expr, else_ : Expr } Value
interpretIf =
    \env { cond, then_, else_ } ->
        Interpreter.do (interpretExpr env cond) <|
            \env1 cond_ ->
                case cond_ of
                    VBool True ->
                        interpretExpr env1 then_

                    VBool False ->
                        interpretExpr env1 else_

                    _ ->
                        Outcome.fail IfConditionNotBool


interpretCase : Interpreter { subject : Expr, branches : List CaseBranch } Value
interpretCase =
    \env { subject, branches } ->
        Interpreter.do (interpretExpr env subject) <|
            \env1 subject_ ->
                let
                    flatBranches : List ( Pattern, Expr )
                    flatBranches =
                        branches
                            |> List.concatMap
                                (\branch ->
                                    branch.orPatterns
                                        |> List.map (\pattern -> ( pattern, branch.body ))
                                )
                in
                interpretCaseBranches env1 ( subject_, flatBranches )


interpretCaseBranches : Interpreter ( Value, List ( Pattern, Expr ) ) Value
interpretCaseBranches =
    \env ( subject, branches ) ->
        case branches of
            [] ->
                Outcome.fail NoCaseBranchMatched

            ( pattern, body ) :: rest ->
                Interpreter.do (interpretPattern env ( pattern, subject )) <|
                    \env1 maybeAdditions ->
                        case maybeAdditions of
                            Nothing ->
                                interpretCaseBranches env ( subject, rest )

                            Just additions ->
                                let
                                    newEnv =
                                        env1 |> Env.addDict additions
                                in
                                interpretExpr newEnv body


interpretList : Interpreter (List Expr) Value
interpretList =
    Interpreter.traverse interpretExpr
        |> Interpreter.map VList


interpretTuple : Interpreter (List Expr) Value
interpretTuple =
    Interpreter.traverse interpretExpr
        |> Interpreter.map VTuple


interpretRecord : Interpreter (List RecordExprContent) Value
interpretRecord =
    Interpreter.traverse interpretRecordExprContent
        |> Interpreter.map
            (\contents ->
                contents
                    |> List.concat
                    |> -- TODO which fields are we throwing away on collision - older or newer?
                       -- TODO should we throw an error instead? Perhaps in typechecker
                       Dict.fromList
                    |> VRecord
            )


interpretRecordExprContent : Interpreter RecordExprContent (List ( String, Value ))
interpretRecordExprContent =
    \env content ->
        case content of
            Field { field, expr } ->
                interpretExpr env expr
                    |> Outcome.map (\value -> [ ( field, value ) ])

            Pun field ->
                Debug.todo "interpret record pun content"

            Spread id ->
                Debug.todo "interpret record spread content"


interpretModule : Interpreter { mod : ModuleModifier, name : String, decls : List Decl } ()
interpretModule =
    \env { mod, name, decls } ->
        -- TODO handle the modifier
        let
            envWithOpenModule : Maybe (Env Value)
            envWithOpenModule =
                env
                    |> Env.createModule name
                    |> Env.open [ name ]
        in
        case envWithOpenModule of
            Nothing ->
                -- Should never happen
                Outcome.fail <| ExpectedModule name

            Just envWithOpenModule_ ->
                interpretProgram envWithOpenModule_ decls
                    |> Outcome.attemptMapEnv Env.goUp ExpectedParent


interpretPrintln : Interpreter Value ()
interpretPrintln =
    \env value ->
        NeedsEffect0 (Effect.Println (Value.toString value)) <|
            \() -> Outcome.succeed env ()
