module Interpreter exposing (interpretProgram)

import AST exposing (..)
import Bitwise
import Debug.Extra as Debug
import Dict exposing (Dict)
import Effect
import Env exposing (Env)
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


type PatternAddition
    = AddNothing
    | AddValue String Value
    | AddBinaryOp BinaryOp Value
    | AddUnaryOp UnaryOp Value
    | ManyAdditions (List PatternAddition)


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
                interpretStatement ToplevelIO env stmt

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

            DExtendModule r ->
                Debug.todo "interpretDecl: DExtendModule"


interpretUnitTest : Interpreter { name : Maybe String, expr : Expr } ()
interpretUnitTest =
    \env { name, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretUnitTest" ()
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretParameterizedTest : Interpreter { name : Maybe String, table : List Expr, args : List Pattern, expr : Expr } ()
interpretParameterizedTest =
    \env { name, table, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretParameterizedTest" ()
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretPropertyTypeTest : Interpreter { name : Maybe String, types : Type, args : List Pattern, expr : Expr } ()
interpretPropertyTypeTest =
    \env { name, types, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretPropertyTypeTest" ()
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretPropertyGenTest : Interpreter { name : Maybe String, gens : Expr, args : List Pattern, expr : Expr } ()
interpretPropertyGenTest =
    \env { name, gens, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretPropertyGenTest" ()
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretStatement : StmtMonad -> Interpreter Stmt ()
interpretStatement monad =
    \env stmt ->
        case stmt of
            SLet r ->
                interpretLet env r

            SLetBang r ->
                case monad of
                    Pure ->
                        Outcome.fail EffectfulStmtInPureBlock_

                    ToplevelIO ->
                        interpretToplevelLetBang env r

            SBang bang ->
                case monad of
                    Pure ->
                        Outcome.fail EffectfulStmtInPureBlock_

                    ToplevelIO ->
                        interpretToplevelBang env bang
                            -- Throw the result away
                            |> Outcome.map (\_ -> ())

            SFunctionDef r ->
                interpretFunctionDef env r

            SBinaryOperatorDef r ->
                interpretBinaryOpDef env r

            SUnaryOperatorDef r ->
                interpretUnaryOpDef env r

            SValueAnnotation r ->
                interpretValueAnnotation env r

            SBinaryOperatorAnnotation r ->
                interpretBinaryOpAnnotation env r

            SUnaryOperatorAnnotation r ->
                interpretUnaryOpAnnotation env r

            SUseModule r ->
                interpretUseModule env r


interpretFunctionDef : Interpreter { name : String, args : List Pattern, body : Expr } ()
interpretFunctionDef =
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


interpretBinaryOpDef : Interpreter { op : BinaryOp, left : Pattern, right : Pattern, body : Expr } ()
interpretBinaryOpDef =
    \env { op, left, right, body } ->
        let
            newEnv =
                Env.addBinaryOp (AST.binaryOpName op)
                    (VClosure
                        { args = [ left, right ]
                        , body = body
                        , env = env
                        }
                    )
                    env
        in
        -- TODO make type annotations do something
        Outcome.succeed newEnv ()


interpretUnaryOpDef : Interpreter { op : UnaryOp, arg : Pattern, body : Expr } ()
interpretUnaryOpDef =
    \env { op, arg, body } ->
        let
            _ =
                Debug.log "TODO do something in interpretUnaryOpDef" ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretValueAnnotation : Interpreter { mod : LetModifier, name : String, type_ : Type } ()
interpretValueAnnotation =
    \env { mod, name, type_ } ->
        let
            _ =
                Debug.log "TODO do something in interpretValueAnnotation" ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretBinaryOpAnnotation : Interpreter { mod : LetModifier, op : BinaryOp, left : Type, right : Type, ret : Type } ()
interpretBinaryOpAnnotation =
    \env { mod, op, left, right, ret } ->
        let
            _ =
                Debug.log "TODO do something in interpretBinaryOpAnnotation" ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUnaryOpAnnotation : Interpreter { mod : LetModifier, op : UnaryOp, arg : Type, ret : Type } ()
interpretUnaryOpAnnotation =
    \env { mod, op, arg, ret } ->
        let
            _ =
                Debug.log "TODO do something in interpretUnaryOpAnnotation" ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUseModule : Interpreter Id ()
interpretUseModule =
    \env moduleId ->
        let
            _ =
                Debug.log "TODO do something in interpretUseModule" ()
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
            additions : PatternAddition
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
                            AddValue c.name <|
                                VClosure
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
                    |> ManyAdditions

            newEnv : Env Value
            newEnv =
                addToEnv additions env
        in
        Outcome.succeed newEnv ()


addToEnv : PatternAddition -> Env Value -> Env Value
addToEnv addition env =
    case addition of
        AddNothing ->
            env

        AddValue name value ->
            Env.add name value env

        AddBinaryOp op value ->
            Env.addBinaryOp (AST.binaryOpName op) value env

        AddUnaryOp op value ->
            Env.addUnaryOp (AST.unaryOpName op) value env

        ManyAdditions additions ->
            List.foldl addToEnv env additions


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
                Debug.log "TODO do something in interpretTypeAlias" ()
        in
        -- TODO make type aliases do something
        Outcome.succeed env ()


interpretToplevelBang : Interpreter Bang Value
interpretToplevelBang =
    \env bang ->
        case bang of
            BValue expr ->
                interpretToplevelBangValue env expr

            BCall r ->
                interpretToplevelBangCall env r


{-| foo!
-}
interpretToplevelBangValue : Interpreter Expr Value
interpretToplevelBangValue =
    interpretExpr


{-| foo!(1,2,3)
-}
interpretToplevelBangCall : Interpreter { fn : Expr, args : List Expr } Value
interpretToplevelBangCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <|
            \env1 fnVal ->
                Interpreter.do (Interpreter.traverse interpretExpr env1 args) <|
                    \env2 argVals ->
                        interpretCallVal env2 ( fnVal, argVals )


interpretIntrinsicCall : Interpreter ( Intrinsic, List Expr ) Value
interpretIntrinsicCall =
    \env ( intrinsic, args ) ->
        Interpreter.do (Interpreter.traverse interpretExpr env args) <|
            \env1 argVals ->
                interpretIntrinsicCallValues env1 ( intrinsic, argVals )


interpretIntrinsicCallValues : Interpreter ( Intrinsic, List Value ) Value
interpretIntrinsicCallValues =
    \env ( intrinsic, argVals ) ->
        case intrinsic of
            IoPrintln ->
                case argVals of
                    [ arg ] ->
                        interpretPrintln env arg
                            |> Outcome.map (\() -> VIo VUnit)

                    _ ->
                        Outcome.fail UnexpectedArity

            IoPure ->
                case argVals of
                    [ arg ] ->
                        Outcome.succeed env (VIo arg)

                    _ ->
                        Outcome.fail UnexpectedArity

            IoBind ->
                case argVals of
                    [ ioVal, fn ] ->
                        let
                            valueInIo =
                                unwrapIo ioVal
                        in
                        interpretCallVal env ( fn, [ valueInIo ] )

                    _ ->
                        Outcome.fail UnexpectedArity


unwrapIo : Value -> Value
unwrapIo ioVal =
    case ioVal of
        VIo valueInIo ->
            valueInIo

        _ ->
            Debug.todo <| "We expected VIo here... is this a bug? " ++ Debug.toString ioVal


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
        Interpreter.do (interpretExpr env expr) <|
            \env1 value ->
                Interpreter.do (interpretPattern env1 ( lhs, value )) <|
                    \env2 envAdditions ->
                        case envAdditions of
                            Nothing ->
                                Outcome.fail <| PatternDidNotMatch ( lhs, value )

                            Just additions ->
                                Outcome.succeed
                                    (addToEnv additions env2)
                                    ()


interpretToplevelLetBang :
    Interpreter
        { mod : LetModifier
        , lhs : Pattern
        , type_ : Maybe Type
        , bang : Bang
        }
        ()
interpretToplevelLetBang =
    \env { lhs, bang } ->
        -- TODO interpret the mod
        -- TODO interpret the type_
        interpretToplevelBang env bang
            |> Outcome.map (\value -> ( lhs, unwrapIo value ))
            |> Interpreter.andThen interpretPattern
            |> Outcome.mapBoth
                (\env_ envAdditions ->
                    case envAdditions of
                        Nothing ->
                            Debug.todo "Pattern didn't match the expr. TODO Report this as user error?"

                        Just additions ->
                            ( addToEnv additions env_
                            , ()
                            )
                )


{-| Returns (NEW) env additions, instead of the whole env.
Nothing is returned if the pattern doesn't match the expr.
-}
interpretPattern : Interpreter ( Pattern, Value ) (Maybe PatternAddition)
interpretPattern =
    \env ( pattern, value ) ->
        case pattern of
            PVar var ->
                AddValue var value
                    |> Just
                    |> Outcome.succeed env

            PInt int ->
                Outcome.succeed env <|
                    case value of
                        VInt n ->
                            if int == n then
                                Just AddNothing

                            else
                                Nothing

                        _ ->
                            Nothing

            PFloat float ->
                Outcome.succeed env <|
                    case value of
                        VFloat n ->
                            if float == n then
                                Just AddNothing

                            else
                                Nothing

                        _ ->
                            Nothing

            PRecordSpread ->
                case value of
                    VRecord fields ->
                        fields
                            |> Dict.toList
                            |> List.map (\( field, val ) -> AddValue field val)
                            |> ManyAdditions
                            |> Just
                            |> Outcome.succeed env

                    VTuple values ->
                        Debug.todo "interpret pattern {..} for tuple"

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
                            |> List.map (\( field, val ) -> AddValue field val)
                            |> ManyAdditions
                            |> Just
                            |> Outcome.succeed env

                    VTuple values ->
                        Debug.todo "interpret pattern {x,y,z} for tuple"

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
                                            Outcome.succeed env (Just (ManyAdditions additions))

                        else
                            Outcome.succeed env Nothing

                    _ ->
                        Outcome.succeed env Nothing

            PTuple ps ->
                case value of
                    VTuple vs ->
                        interpretPatternTuple env ( ps, vs )

                    _ ->
                        Outcome.succeed env Nothing

            PList ps ->
                case value of
                    VList vs ->
                        interpretPatternList env ( ps, vs )

                    _ ->
                        Outcome.succeed env Nothing

            PWildcard ->
                Outcome.succeed env (Just AddNothing)

            PUnit ->
                Outcome.succeed env <|
                    case value of
                        VUnit ->
                            Just AddNothing

                        _ ->
                            Nothing

            PSpread spread ->
                Debug.todo <| "interpret pattern spread: " ++ Debug.toString spread

            PUnaryOpDef unaryOp ->
                case value of
                    VClosure r ->
                        Outcome.succeed env (Just (AddUnaryOp unaryOp value))

                    _ ->
                        Outcome.succeed env Nothing

            PBinaryOpDef binaryOp ->
                case value of
                    VClosure r ->
                        Outcome.succeed env (Just (AddBinaryOp binaryOp value))

                    _ ->
                        Outcome.succeed env Nothing


interpretPatternTuple : Interpreter ( List Pattern, List Value ) (Maybe PatternAddition)
interpretPatternTuple =
    \env ( ps, vs ) ->
        if List.length ps /= List.length vs then
            Outcome.succeed env Nothing

        else
            let
                pairs =
                    List.map2 Tuple.pair ps vs
            in
            Interpreter.do (Interpreter.traverse interpretPattern env pairs) <|
                \env1 maybeAdditions ->
                    case Maybe.combine maybeAdditions of
                        Nothing ->
                            Outcome.succeed env1 Nothing

                        Just additions ->
                            Outcome.succeed env1 (Just (ManyAdditions additions))


interpretPatternList : Interpreter ( List Pattern, List Value ) (Maybe PatternAddition)
interpretPatternList =
    \env ( ps, vs ) ->
        case List.length (List.filter AST.isSpreadPattern ps) of
            0 ->
                Debug.todo "interpretPatternList - zero spreads"

            1 ->
                case ps of
                    (PSpread a) :: rest ->
                        Debug.todo "interpretPatternList - one spread - spread at the beginning"

                    _ ->
                        case List.reverse ps of
                            (PSpread a) :: rest ->
                                let
                                    _ =
                                        Debug.log "TODO - interpretPatternList - one spread - spread at the end"
                                in
                                Outcome.succeed env (Just AddNothing)

                            _ ->
                                Debug.todo "interpretPatternList - one spread - spread somewhere in the middle"

            _ ->
                Outcome.fail MultipleSpreadPatterns


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

            Char s ->
                Outcome.succeed env (VChar s)

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


interpretCall : Interpreter { fn : Expr, args : List Expr } Value
interpretCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <|
            \env1 fnVal ->
                Interpreter.do (Interpreter.traverse interpretExpr env1 args) <|
                    \env2 argVals ->
                        interpretCallVal env2 ( fnVal, argVals )


interpretCallVal : Interpreter ( Value, List Value ) Value
interpretCallVal =
    \env ( fnVal, argVals ) ->
        case fnVal of
            VRecordGetter field ->
                case argVals of
                    [ VTuple values ] ->
                        interpretRecordGetTuple env ( values, field )

                    [ VRecord record ] ->
                        Debug.todo "call record getter on a record"

                    _ ->
                        Outcome.fail CallingRecordGetterOnNonRecord

            VClosure r ->
                let
                    argsLength =
                        List.length r.args

                    valsLength =
                        List.length argVals
                in
                case compare argsLength valsLength of
                    LT ->
                        {- Less args than values

                           It's possible the result of the function is going to
                           be another function, so let's just go through with it
                           until what we're trying to call is definitely
                           not a function.

                           (Until we typecheck we won't know!)
                        -}
                        if argsLength == 0 then
                            Outcome.fail CallingNonFunction

                        else
                            let
                                ( doableVals, valsRest ) =
                                    List.splitAt argsLength argVals
                            in
                            Interpreter.do (interpretCallVal env ( fnVal, doableVals )) <|
                                \env2 resultVal ->
                                    interpretCallVal env2 ( resultVal, valsRest )

                    GT ->
                        -- More args than values
                        -- We'll partially apply the function and return a new lambda waiting for the rest
                        let
                            availablePairs : List ( Pattern, Value )
                            availablePairs =
                                List.map2 Tuple.pair r.args argVals

                            argsRest : List Pattern
                            argsRest =
                                r.args
                                    |> List.drop valsLength
                        in
                        Interpreter.do (Interpreter.traverse interpretPattern env availablePairs) <|
                            \env2 maybeAdditions ->
                                case Maybe.combine maybeAdditions of
                                    Nothing ->
                                        Outcome.fail PatternMismatch

                                    Just additions ->
                                        Outcome.succeed env2 <|
                                            VClosure
                                                { args = argsRest
                                                , body = r.body
                                                , env = List.foldl addToEnv r.env additions
                                                }

                    EQ ->
                        let
                            pairs : List ( Pattern, Value )
                            pairs =
                                List.map2 Tuple.pair r.args argVals
                        in
                        Interpreter.do (Interpreter.traverse interpretPattern env pairs) <|
                            \env2 maybeAdditions ->
                                case Maybe.combine maybeAdditions of
                                    Nothing ->
                                        Outcome.fail PatternMismatch

                                    Just additions ->
                                        Interpreter.do (interpretExpr (List.foldl addToEnv r.env additions) r.body) <|
                                            \_ callResult ->
                                                Outcome.succeed env2 callResult

            VIntrinsic intrinsic ->
                interpretIntrinsicCallValues env ( intrinsic, argVals )

            _ ->
                Outcome.fail CallingNonFunction


interpretLambda : Interpreter { args : List Pattern, body : Expr } Value
interpretLambda =
    \env { args, body } ->
        Outcome.succeed env <| VClosure { args = args, body = body, env = env }


type StmtMonad
    = ToplevelIO
    | Pure


interpretBlock : Interpreter { stmts : List Stmt, ret : Expr } Value
interpretBlock =
    \env { stmts, ret } ->
        Interpreter.do (Interpreter.traverse (interpretStatement Pure) env stmts) <|
            \env1 _ ->
                interpretExpr env1 ret


interpretEffectBlock : Interpreter { monadModule : List String, stmts : List Stmt, ret : BangOrExpr } Value
interpretEffectBlock =
    \env { monadModule, stmts, ret } ->
        let
            pure : Expr -> Expr
            pure expr =
                Call
                    { fn = Identifier (Id.pure monadModule)
                    , args = [ expr ]
                    }

            bind : Expr -> Pattern -> Expr -> Expr
            bind monadicExpr lhs kont =
                Call
                    { fn = Identifier (Id.bind monadModule)
                    , args =
                        [ monadicExpr
                        , Lambda
                            { args = [ lhs ]
                            , body = kont
                            }
                        ]
                    }

            wrapInMonad : BangOrExpr -> Expr
            wrapInMonad boe =
                case boe of
                    -- foo -> Monad.pure(foo)
                    E expr ->
                        pure expr

                    -- foo! -> foo
                    B (BValue expr) ->
                        expr

                    -- foo!(1,2) --> foo(1,2)
                    B (BCall { fn, args }) ->
                        Call { fn = fn, args = args }

            blockExpr : Expr
            blockExpr =
                -- Here we fold an entire effect block into a pure+bind chain.
                -- This is the do-notation desugaring
                List.foldr
                    addLayer
                    (wrapInMonad ret)
                    stmts

            addLayer : Stmt -> Expr -> Expr
            addLayer stmt innerExpr =
                case stmt of
                    -- TODO do something about mod,type? or disallow them in parser or make a separate EffectBlockStmt that doesn't hold them?
                    -- x = foo --> pure(foo) >>= (\x -> ...)
                    SLet { lhs, expr } ->
                        bind (wrapInMonad (E expr)) lhs innerExpr

                    -- TODO do something about mod,type? or disallow them in parser or make a separate EffectBlockStmt that doesn't hold them?
                    -- x = foo! --> foo >>= (\x -> ...)
                    SLetBang { lhs, bang } ->
                        bind (wrapInMonad (B bang)) lhs innerExpr

                    -- foo! --> foo >>= (\_ -> ...)
                    SBang bang ->
                        bind (wrapInMonad (B bang)) PWildcard innerExpr

                    SFunctionDef { name, args, body } ->
                        addLayer
                            (SLet
                                { lhs = PVar name
                                , expr = Lambda { args = args, body = body }
                                , mod = LetNoModifier
                                , type_ = Nothing
                                }
                            )
                            innerExpr

                    SUnaryOperatorDef { op, arg, body } ->
                        addLayer
                            (SLet
                                { lhs = PUnaryOpDef op
                                , expr = Lambda { args = [ arg ], body = body }
                                , mod = LetNoModifier
                                , type_ = Nothing
                                }
                            )
                            innerExpr

                    SBinaryOperatorDef { op, left, right, body } ->
                        addLayer
                            (SLet
                                { lhs = PBinaryOpDef op
                                , expr = Lambda { args = [ left, right ], body = body }
                                , mod = LetNoModifier
                                , type_ = Nothing
                                }
                            )
                            innerExpr

                    SValueAnnotation r ->
                        let
                            _ =
                                Debug.log "TODO do something in interpretEffectBlock - SValueAnnotation" ()
                        in
                        innerExpr

                    SUnaryOperatorAnnotation r ->
                        let
                            _ =
                                Debug.log "TODO do something in interpretEffectBlock - SUnaryOperatorAnnotation" ()
                        in
                        innerExpr

                    SBinaryOperatorAnnotation r ->
                        let
                            _ =
                                Debug.log "TODO do something in interpretEffectBlock - SBinaryOperatorAnnotation" ()
                        in
                        innerExpr

                    SUseModule m ->
                        Debug.todo "interpretEffecftBlock - SUseModule"
        in
        interpretExpr env blockExpr


interpretToplevelBangOrExpr : Interpreter BangOrExpr Value
interpretToplevelBangOrExpr =
    \env boe ->
        case boe of
            B bang ->
                interpretToplevelBang env bang

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
            -- language-given unary operators
            ( NegateNum, VInt n ) ->
                Outcome.succeed env <| VInt (negate n)

            ( NegateNum, VFloat n ) ->
                Outcome.succeed env <| VFloat (negate n)

            ( NegateBool, VBool b ) ->
                Outcome.succeed env <| VBool (not b)

            ( NegateBin, VInt n ) ->
                Outcome.succeed env <| VInt (Bitwise.complement n)

            -- user-given binary operators
            _ ->
                let
                    finishWithUnknown () =
                        Debug.todo <| "interpretUnaryOp: " ++ Debug.toString ( op, val ) ++ " (and not found in user unary ops)"
                in
                case Env.getUnaryOp (AST.unaryOpName op) env of
                    Nothing ->
                        finishWithUnknown ()

                    Just userOverloads ->
                        interpretUnaryOpCallUser finishWithUnknown env ( userOverloads, val )


interpretUnaryOpCallUser : (() -> Outcome Value) -> Interpreter ( List Value, Value ) Value
interpretUnaryOpCallUser finishWithUnknown =
    \env ( userOverloads, arg ) ->
        -- TODO pick the correct overload based on the types.
        -- Right now we try them all and pick the first one that doesn't error out
        case userOverloads of
            [] ->
                finishWithUnknown ()

            overload :: rest ->
                interpretCallVal env ( overload, [ arg ] )
                    |> Outcome.onError (\_ -> interpretUnaryOpCallUser finishWithUnknown env ( rest, arg ))


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
            -- language-given binary operators
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

            -- user-given binary operators
            _ ->
                let
                    finishWithUnknown () =
                        Debug.todo <| "interpretBinaryOp: " ++ Debug.toString ( left, op, right ) ++ " (and not found in user binary ops)"
                in
                case Env.getBinaryOp (AST.binaryOpName op) env of
                    Nothing ->
                        finishWithUnknown ()

                    Just userOverloads ->
                        interpretBinaryOpCallUser finishWithUnknown env ( left, userOverloads, right )


interpretBinaryOpCallUser : (() -> Outcome Value) -> Interpreter ( Value, List Value, Value ) Value
interpretBinaryOpCallUser finishWithUnknown =
    \env ( left, userOverloads, right ) ->
        -- TODO pick the correct overload based on the types.
        -- Right now we try them all and pick the first one that doesn't error out
        case userOverloads of
            [] ->
                finishWithUnknown ()

            overload :: rest ->
                interpretCallVal env ( overload, [ left, right ] )
                    |> Outcome.onError (\_ -> interpretBinaryOpCallUser finishWithUnknown env ( left, rest, right ))


interpretRecordGet : Interpreter { record : Expr, field : String } Value
interpretRecordGet =
    \env { record, field } ->
        Interpreter.do (interpretExpr env record) <|
            \env1 recordVal ->
                case recordVal of
                    VTuple values ->
                        interpretRecordGetTuple env1 ( values, field )

                    VRecord fields ->
                        case Dict.get field fields of
                            Nothing ->
                                Outcome.fail <| RecordFieldNotFound field

                            Just content ->
                                Outcome.succeed env1 content

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
                        --let
                        --    _ =
                        --        Debug.log ("Not found in:\n" ++ Env.toString { valueToString = Value.toString } env) id
                        --in
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
                                        addToEnv additions env1
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
