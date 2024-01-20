module Interpreter exposing (interpretProgram)

import AST.Frontend as AST exposing (..)
import Basics.Extra as Basics
import Bitwise
import Debug.Extra
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
                interpretExtendModule env r


interpretExtendModule : Interpreter { module_ : List String, decls : List Decl } ()
interpretExtendModule =
    \env { module_, decls } ->
        let
            envWithOpenModule : Maybe (Env Value)
            envWithOpenModule =
                Env.goInto module_ env
        in
        case envWithOpenModule of
            Nothing ->
                -- Should never happen
                Outcome.fail <| ExpectedModule (String.join "." module_)

            Just envWithOpenModule_ ->
                interpretProgram envWithOpenModule_ decls
                    |> Basics.doNTimes (List.length module_) (Outcome.attemptMapEnv Env.goUp ExpectedParent)


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
                interpretLet Pure env r

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
                Env.addBinaryOp (AST.binaryOp op)
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

            modulePath =
                moduleId.qualifiers ++ [ moduleId.name ]
        in
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
            Env.addBinaryOp (AST.binaryOp op) value env

        AddUnaryOp op value ->
            Env.addUnaryOp (AST.unaryOp op) value env

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
    interpretExpr ToplevelIO


{-| foo!(1,2,3)
-}
interpretToplevelBangCall : Interpreter { fn : Expr, args : List Expr } Value
interpretToplevelBangCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr ToplevelIO env fn) <| \env1 fnVal ->
        Interpreter.do (Interpreter.traverse (interpretExpr ToplevelIO) env1 args) <| \env2 argVals ->
        interpretCallVal ToplevelIO env2 ( fnVal, argVals )


interpretIntrinsicCallValues : StmtMonad -> Interpreter ( Intrinsic, List Value ) Value
interpretIntrinsicCallValues stmtMonad =
    \env ( intrinsic, argVals ) ->
        -- TODO this will need something smarter, surely
        case stmtMonad of
            Pure ->
                let
                    delay =
                        Outcome.succeed env (VIntrinsicCall intrinsic argVals)
                in
                {- Don't run these yet - keep them around as "recipes"
                   Example:

                     nl = IO.println("========")
                     IO.println!("x")
                     nl!
                     nl!
                     IO.println!("y")

                   This should print:

                     x
                     ========
                     ========
                     y

                   While if we ran the println early, we'd get something like:

                     ========
                     x
                     y

                -}
                case intrinsic of
                    IoPrintln ->
                        delay

                    IoInspect ->
                        delay

                    IoToInspectString ->
                        delay

                    IoToString ->
                        delay

                    IoPure ->
                        delay

                    IoBind ->
                        delay

                    IoReadFile ->
                        delay

                    IoWriteFile ->
                        delay

            ToplevelIO ->
                case intrinsic of
                    IoPrintln ->
                        case argVals of
                            [ arg ] ->
                                interpretPrintln env arg
                                    |> Outcome.map (\() -> VIo VUnit)

                            _ ->
                                Outcome.fail UnexpectedArity

                    IoInspect ->
                        case argVals of
                            [ arg ] ->
                                interpretInspect env arg
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
                            [ value, fn ] ->
                                unwrapIo stmtMonad env value
                                    |> Outcome.andThen
                                        (\env2 valueInIo ->
                                            interpretCallVal stmtMonad env2 ( fn, [ valueInIo ] )
                                        )

                            _ ->
                                Outcome.fail UnexpectedArity

                    IoToInspectString ->
                        case argVals of
                            [ arg ] ->
                                Outcome.succeed env (VString (Value.toInspectString arg))

                            _ ->
                                Outcome.fail UnexpectedArity

                    IoToString ->
                        case argVals of
                            [ arg ] ->
                                Outcome.succeed env (VString (Value.toShowString arg))

                            _ ->
                                Outcome.fail UnexpectedArity

                    IoReadFile ->
                        case argVals of
                            [ arg ] ->
                                interpretReadFile env arg
                                    |> Outcome.map
                                        (\maybeStr ->
                                            case maybeStr of
                                                Nothing ->
                                                    VIo Value.nothing

                                                Just str ->
                                                    VIo (Value.just (VString str))
                                        )

                            _ ->
                                Outcome.fail UnexpectedArity

                    IoWriteFile ->
                        case argVals of
                            [ recordArg ] ->
                                interpretWriteFile env recordArg
                                    |> Outcome.map (VIo << VBool)

                            _ ->
                                Outcome.fail UnexpectedArity


unwrapIo : StmtMonad -> Interpreter Value Value
unwrapIo stmtMonad =
    \env value ->
        case value of
            VIo valueInIo ->
                Outcome.succeed env valueInIo

            VIntrinsicCall intrinsic values ->
                interpretIntrinsicCallValues stmtMonad env ( intrinsic, values )
                    |> Outcome.andThen
                        (\env2 finalValue ->
                            case finalValue of
                                VIo valueInIo ->
                                    Outcome.succeed env2 valueInIo

                                _ ->
                                    Debug.todo "We expected VIo (only) here... is this a bug?"
                        )

            _ ->
                Debug.todo <|
                    "We expected VIo or VIntrinsicCall here... is this a bug? "
                        ++ Value.toInspectString value


interpretLet :
    StmtMonad
    ->
        Interpreter
            { mod : LetModifier
            , lhs : Pattern
            , type_ : Maybe Type
            , expr : Expr
            }
            ()
interpretLet stmtMonad =
    \env { lhs, expr } ->
        -- TODO interpret the modifier
        -- TODO interpret the type
        Interpreter.do (interpretExpr stmtMonad env expr) <| \env1 value ->
        Interpreter.do (interpretPattern stmtMonad env1 ( lhs, value )) <| \env2 envAdditions ->
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
        Interpreter.do (interpretToplevelBang env bang) <| \env2 value ->
        Interpreter.do (unwrapIo ToplevelIO env2 value) <| \env3 valueInIo ->
        interpretPattern ToplevelIO env3 ( lhs, valueInIo )
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
interpretPattern : StmtMonad -> Interpreter ( Pattern, Value ) (Maybe PatternAddition)
interpretPattern stmtMonad =
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

            PChar c ->
                Outcome.succeed env <|
                    case value of
                        VChar v ->
                            if c == v then
                                Just AddNothing

                            else
                                Nothing

                        _ ->
                            Nothing

            PString s ->
                Outcome.succeed env <|
                    case value of
                        VString v ->
                            if s == v then
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
                        interpretPattern stmtMonad env ( PRecordSpread, VRecord (tupleToNumericRecord values) )

                    _ ->
                        Debug.todo <| "interpret pattern {..} - other: " ++ Value.toInspectString value

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
                        if r.id.name == id.name then
                            Interpreter.do (interpretIdentifier stmtMonad env id) <| \env1 found ->
                            let
                                handleVConstructor : Interpreter { id : Id, args : List Value } (Maybe PatternAddition)
                                handleVConstructor =
                                    \envX vc ->
                                        if vc == r then
                                            let
                                                pairs : List ( Pattern, Value )
                                                pairs =
                                                    List.map2 Tuple.pair args r.args
                                            in
                                            Interpreter.do (Interpreter.traverse (interpretPattern stmtMonad) envX pairs) <| \envX1 maybeAdditions ->
                                            case Maybe.combine maybeAdditions of
                                                Nothing ->
                                                    Outcome.succeed envX1 Nothing

                                                Just additions ->
                                                    Outcome.succeed envX1 (Just (ManyAdditions additions))

                                        else
                                            Outcome.succeed envX Nothing
                            in
                            case found of
                                VConstructor found_ ->
                                    handleVConstructor env1 found_

                                VClosure c ->
                                    Interpreter.do (interpretCallVal stmtMonad env1 ( found, r.args )) <| \env2 result ->
                                    case result of
                                        VConstructor found_ ->
                                            handleVConstructor env2 found_

                                        _ ->
                                            Debug.todo <| "Unexpected result: " ++ Debug.toString result

                                _ ->
                                    Outcome.succeed env1 Nothing

                        else
                            Outcome.succeed env Nothing

                    _ ->
                        Outcome.succeed env Nothing

            PTuple ps ->
                case value of
                    VTuple vs ->
                        interpretPatternTuple stmtMonad env ( ps, vs )

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
                Debug.todo <| "interpret pattern spread: " ++ AST.patternToString pattern

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

            PAs _ _ ->
                Debug.todo <| "interpret as-pattern: " ++ AST.patternToString pattern


interpretPatternTuple : StmtMonad -> Interpreter ( List Pattern, List Value ) (Maybe PatternAddition)
interpretPatternTuple stmtMonad =
    \env ( ps, vs ) ->
        if List.length ps /= List.length vs then
            Outcome.succeed env Nothing

        else
            let
                pairs =
                    List.map2 Tuple.pair ps vs
            in
            Interpreter.do (Interpreter.traverse (interpretPattern stmtMonad) env pairs) <| \env1 maybeAdditions ->
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


interpretExpr : StmtMonad -> Interpreter Expr Value
interpretExpr stmtMonad =
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
                interpretIdentifier stmtMonad env id

            RootIdentifier id ->
                interpretRootIdentifier env id

            Constructor_ r ->
                interpretConstructor stmtMonad env r

            Bool bool ->
                Outcome.succeed env (VBool bool)

            Unit ->
                Outcome.succeed env VUnit

            If r ->
                interpretIf stmtMonad env r

            Case r ->
                interpretCase stmtMonad env r

            List xs ->
                interpretList stmtMonad env xs

            Tuple xs ->
                interpretTuple stmtMonad env xs

            Record fields ->
                interpretRecord stmtMonad env fields

            UnaryOp op e ->
                interpretUnaryOpCall stmtMonad env ( op, e )

            BinaryOp left op right ->
                interpretBinaryOpCall stmtMonad env ( left, op, right )

            RecordGet r ->
                interpretRecordGet stmtMonad env r

            RecordGetter field ->
                Outcome.succeed env (VRecordGetter field)

            Call r ->
                interpretCall stmtMonad env r

            Lambda r ->
                interpretLambda env r

            Block r ->
                interpretBlock env r

            EffectBlock r ->
                interpretEffectBlock stmtMonad env r


interpretCall : StmtMonad -> Interpreter { fn : Expr, args : List Expr } Value
interpretCall stmtMonad =
    \env { fn, args } ->
        Interpreter.do (interpretExpr stmtMonad env fn) <| \env1 fnVal ->
        Interpreter.do (Interpreter.traverse (interpretExpr stmtMonad) env1 args) <| \env2 argVals ->
        interpretCallVal stmtMonad env2 ( fnVal, argVals )


interpretCallVal : StmtMonad -> Interpreter ( Value, List Value ) Value
interpretCallVal stmtMonad =
    \env ( fnVal, argVals ) ->
        case fnVal of
            VRecordGetter field ->
                case argVals of
                    [ VTuple values ] ->
                        interpretRecordGetDict env ( field, tupleToNumericAndWordyRecord values )

                    [ VRecord fields ] ->
                        interpretRecordGetDict env ( field, fields )

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
                            Interpreter.do (interpretCallVal stmtMonad env ( fnVal, doableVals )) <| \env2 resultVal ->
                            interpretCallVal stmtMonad env2 ( resultVal, valsRest )

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
                        Interpreter.do (Interpreter.traverse (interpretPattern stmtMonad) env availablePairs) <| \env2 maybeAdditions ->
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
                        Interpreter.do (Interpreter.traverse (interpretPattern stmtMonad) env pairs) <| \env2 maybeAdditions ->
                        case Maybe.combine maybeAdditions of
                            Nothing ->
                                Outcome.fail PatternMismatch

                            Just additions ->
                                Interpreter.do (interpretExpr stmtMonad (List.foldl addToEnv r.env additions) r.body) <| \_ callResult ->
                                Outcome.succeed env2 callResult

            VIntrinsic intrinsic ->
                interpretIntrinsicCallValues stmtMonad env ( intrinsic, argVals )

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
    -- TODO should we do this (enforce Pure), instead of grabbing the StmtMonad from the parent scope?
    \env { stmts, ret } ->
        Interpreter.do (Interpreter.traverse (interpretStatement Pure) env stmts) <| \env1 _ ->
        interpretExpr Pure env1 ret


interpretEffectBlock : StmtMonad -> Interpreter { monadModule : List String, stmts : List Stmt, ret : BangOrExpr } Value
interpretEffectBlock stmtMonad =
    -- TODO make our own StmtMonad from monadModule? But make sure it's not IO? IDK, think about this further
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
                        let
                            _ =
                                Debug.log "TODO do something in interpretEffectBlock - SUseModule" ()
                        in
                        innerExpr
        in
        interpretExpr stmtMonad env blockExpr


interpretToplevelBangOrExpr : Interpreter BangOrExpr Value
interpretToplevelBangOrExpr =
    \env boe ->
        case boe of
            B bang ->
                interpretToplevelBang env bang

            E expr ->
                interpretExpr ToplevelIO env expr


interpretUnaryOpCall : StmtMonad -> Interpreter ( UnaryOp, Expr ) Value
interpretUnaryOpCall stmtMonad =
    \env ( op, expr ) ->
        Interpreter.do (interpretExpr stmtMonad env expr) <| \env1 val ->
        interpretUnaryOpCallVal stmtMonad env1 ( op, val )


interpretUnaryOpCallVal : StmtMonad -> Interpreter ( UnaryOp, Value ) Value
interpretUnaryOpCallVal stmtMonad =
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
                        Outcome.fail <| UnknownUnaryOpOverload ( op, val )
                in
                case Env.getUnaryOp (AST.unaryOp op) env of
                    Nothing ->
                        finishWithUnknown ()

                    Just userOverloads ->
                        interpretUnaryOpCallUser stmtMonad finishWithUnknown env ( userOverloads, val )


interpretUnaryOpCallUser : StmtMonad -> (() -> Outcome Value) -> Interpreter ( List Value, Value ) Value
interpretUnaryOpCallUser stmtMonad finishWithUnknown =
    \env ( userOverloads, arg ) ->
        -- TODO pick the correct overload based on the types.
        -- Right now we try them all and pick the first one that doesn't error out
        case userOverloads of
            [] ->
                finishWithUnknown ()

            overload :: rest ->
                interpretCallVal stmtMonad env ( overload, [ arg ] )
                    |> Outcome.onError (\_ -> interpretUnaryOpCallUser stmtMonad finishWithUnknown env ( rest, arg ))


interpretBinaryOpCall : StmtMonad -> Interpreter ( Expr, BinaryOp, Expr ) Value
interpretBinaryOpCall stmtMonad =
    \env ( left, op, right ) ->
        Interpreter.do (interpretExpr stmtMonad env left) <| \env1 leftVal ->
        Interpreter.do (interpretExpr stmtMonad env1 right) <| \env2 rightVal ->
        interpretBinaryOpCallVal stmtMonad env2 ( leftVal, op, rightVal )


interpretBinaryOpCallVal : StmtMonad -> Interpreter ( Value, BinaryOp, Value ) Value
interpretBinaryOpCallVal stmtMonad =
    \env ( left, op, right ) ->
        case ( left, op, right ) of
            -- language-given binary operators
            -- Plus: ints, floats
            ( VInt a, Plus, VInt b ) ->
                Outcome.succeed env <| VInt (a + b)

            ( VInt a, Plus, VFloat b ) ->
                Outcome.succeed env <| VFloat (toFloat a + b)

            ( VFloat a, Plus, VInt b ) ->
                Outcome.succeed env <| VFloat (a + toFloat b)

            ( VFloat a, Plus, VFloat b ) ->
                Outcome.succeed env <| VFloat (a + b)

            -- Minus: ints, floats
            ( VInt a, Minus, VInt b ) ->
                Outcome.succeed env <| VInt (a - b)

            ( VFloat a, Minus, VFloat b ) ->
                Outcome.succeed env <| VFloat (a - b)

            -- Times: ints, floats, lists
            ( VInt a, Times, VInt b ) ->
                Outcome.succeed env <| VInt (a * b)

            ( VInt a, Times, VFloat b ) ->
                Outcome.succeed env <| VFloat (toFloat a * b)

            ( VFloat a, Times, VFloat b ) ->
                Outcome.succeed env <| VFloat (a * b)

            -- Div: ints, floats
            ( VInt a, Div, VInt b ) ->
                Outcome.succeed env <| VInt (a // b)

            ( VFloat a, Div, VFloat b ) ->
                -- TODO handle div by 0? make it be Infinity?
                Outcome.succeed env <| VFloat (a / b)

            -- Mod: ints, floats
            ( VInt a, Mod, VInt b ) ->
                Outcome.succeed env <| VInt (a |> modBy b)

            ( VFloat a, Mod, VInt b ) ->
                let
                    integer =
                        floor a

                    result =
                        toFloat (integer |> modBy b) + a - toFloat integer
                in
                Outcome.succeed env <| VFloat result

            ( VFloat a, Mod, VFloat b ) ->
                Outcome.succeed env <| VFloat (a - b * toFloat (floor (a / b)))

            -- Pow: ints, floats
            ( VInt a, Pow, VInt b ) ->
                Outcome.succeed env <| VInt (a ^ b)

            -- OrBin: ints
            ( VInt a, OrBin, VInt b ) ->
                Outcome.succeed env <| VInt (Bitwise.or a b)

            -- AndBin: ints
            ( VInt a, AndBin, VInt b ) ->
                Outcome.succeed env <| VInt (Bitwise.and a b)

            -- XorBin: ints
            ( VInt a, XorBin, VInt b ) ->
                Outcome.succeed env <| VInt (Bitwise.xor a b)

            -- ShiftL: ints
            ( VInt a, ShiftL, VInt b ) ->
                Outcome.succeed env <| VInt (a |> Bitwise.shiftLeftBy b)

            -- ShiftR: ints
            ( VInt a, ShiftR, VInt b ) ->
                Outcome.succeed env <| VInt (a |> Bitwise.shiftRightBy b)

            -- ShiftRU: ints
            ( VInt a, ShiftRU, VInt b ) ->
                Outcome.succeed env <| VInt (a |> Bitwise.shiftRightZfBy b)

            -- LTE: ints, floats, strings, chars, bools, unit,
            --      lists, tuples, records
            ( VInt a, Lte, VInt b ) ->
                Outcome.succeed env <| VBool (a <= b)

            -- LT: ints, floats, strings, chars, bools, unit,
            --      lists, tuples, records
            ( VInt a, Lt, VInt b ) ->
                Outcome.succeed env <| VBool (a < b)

            -- EQ: ints, floats, strings, chars, bools, unit,
            --     list, tuples, records, record getters, constructors
            ( VInt a, Eq, VInt b ) ->
                Outcome.succeed env <| VBool (a == b)

            ( VList xs, Eq, VList ys ) ->
                if List.length xs /= List.length ys then
                    Outcome.succeed env <| VBool False

                else if List.all Value.isEqualityAllowed (xs ++ ys) then
                    Outcome.succeed env <| VBool (xs == ys)

                else
                    Outcome.fail EquatingNonequatable

            ( VConstructor a, Eq, VConstructor b ) ->
                Outcome.succeed env <| VBool (a == b)

            -- NEQ: ints, floats, strings, chars, bools, unit,
            --     lists, tuples, records, record getters, constructors
            ( VInt a, Neq, VInt b ) ->
                Outcome.succeed env <| VBool (a /= b)

            -- GT: ints, floats, strings, chars, bools, unit,
            --      lists, tuples, records
            ( VInt a, Gt, VInt b ) ->
                Outcome.succeed env <| VBool (a > b)

            -- GTE: ints, floats, strings, chars, bools, unit,
            --      lists, tuples, records
            ( VInt a, Gte, VInt b ) ->
                Outcome.succeed env <| VBool (a >= b)

            -- OrBool: Bool
            ( VBool a, OrBool, VBool b ) ->
                Outcome.succeed env <| VBool (a || b)

            -- AndBool: Bool
            ( VBool a, AndBool, VBool b ) ->
                Outcome.succeed env <| VBool (a && b)

            -- Append: Strings, Lists
            ( VString a, Append, VString b ) ->
                Outcome.succeed env <| VString (a ++ b)

            ( VList a, Append, VList b ) ->
                Outcome.succeed env <| VList (a ++ b)

            ( a, Append, VList b ) ->
                Outcome.succeed env <| VList (a :: b)

            ( VList a, Append, b ) ->
                Outcome.succeed env <| VList (a ++ [ b ])

            -- RangeInclusive: Ints, Chars
            ( VInt a, RangeInclusive, VInt b ) ->
                Outcome.succeed env <| VList (List.range a b |> List.map VInt)

            -- RangeExclusive: Ints, Chars
            ( VInt a, RangeExclusive, VInt b ) ->
                Outcome.succeed env <| VList (List.range a (b - 1) |> List.map VInt)

            -- user-given binary operators
            _ ->
                let
                    finishWithUnknown () =
                        Outcome.fail <| UnknownBinaryOpOverload ( left, op, right )
                in
                case Env.getBinaryOp (AST.binaryOp op) env of
                    Nothing ->
                        finishWithUnknown ()

                    Just userOverloads ->
                        interpretBinaryOpCallUser stmtMonad finishWithUnknown env ( left, userOverloads, right )


interpretBinaryOpCallUser : StmtMonad -> (() -> Outcome Value) -> Interpreter ( Value, List Value, Value ) Value
interpretBinaryOpCallUser stmtMonad finishWithUnknown =
    \env ( left, userOverloads, right ) ->
        -- TODO pick the correct overload based on the types.
        -- Right now we try them all and pick the first one that doesn't error out
        case userOverloads of
            [] ->
                finishWithUnknown ()

            overload :: rest ->
                interpretCallVal stmtMonad env ( overload, [ left, right ] )
                    |> Outcome.onError (\_ -> interpretBinaryOpCallUser stmtMonad finishWithUnknown env ( left, rest, right ))


interpretRecordGet : StmtMonad -> Interpreter { record : Expr, field : String } Value
interpretRecordGet stmtMonad =
    \env { record, field } ->
        Interpreter.do (interpretExpr stmtMonad env record) <| \env1 recordVal ->
        case recordVal of
            VTuple values ->
                interpretTupleGetDict env1 ( field, tupleToNumericAndWordyRecord values )

            VRecord fields ->
                interpretRecordGetDict env1 ( field, fields )

            _ ->
                Outcome.fail CallingRecordGetOnNonRecord


interpretRecordGetDict : Interpreter ( String, Dict String Value ) Value
interpretRecordGetDict =
    \env ( field, fields ) ->
        case Dict.get field fields of
            Nothing ->
                Outcome.fail <| RecordFieldNotFound field

            Just content ->
                Outcome.succeed env content


interpretTupleGetDict : Interpreter ( String, Dict String Value ) Value
interpretTupleGetDict =
    \env ( field, fields ) ->
        case Dict.get field fields of
            Nothing ->
                let
                    err =
                        if isTupleGetter field then
                            AccessingMissingTupleElement field

                        else
                            TupleUnknownField field
                in
                Outcome.fail err

            Just content ->
                Outcome.succeed env content


isTupleGetter : String -> Bool
isTupleGetter field =
    let
        isNumericTupleGetter =
            String.startsWith "el" field
                && String.toInt (String.dropLeft 2 field)
                /= Nothing

        isWordyTupleGetter =
            Set.member field wordyTupleGetters
    in
    isNumericTupleGetter || isWordyTupleGetter


wordyTupleGetters : Set String
wordyTupleGetters =
    List.range 0 9
        |> List.filterMap tupleIndexToWordyField
        |> Set.fromList


tupleIndexToWordyField : Int -> Maybe String
tupleIndexToWordyField n =
    case n of
        0 ->
            Just "first"

        1 ->
            Just "second"

        2 ->
            Just "third"

        3 ->
            Just "fourth"

        4 ->
            Just "fifth"

        5 ->
            Just "sixth"

        6 ->
            Just "seventh"

        7 ->
            Just "eighth"

        8 ->
            Just "ninth"

        9 ->
            Just "tenth"

        _ ->
            Nothing


tupleIndexToNumericField : Int -> String
tupleIndexToNumericField n =
    "el" ++ String.fromInt (n + 1)


{-| Useful for spreads
-}
tupleToNumericRecord : List Value -> Dict String Value
tupleToNumericRecord values =
    values
        |> List.indexedMap (\i value -> ( tupleIndexToNumericField i, value ))
        |> Dict.fromList


{-| Useful for record gets
-}
tupleToNumericAndWordyRecord : List Value -> Dict String Value
tupleToNumericAndWordyRecord values =
    values
        |> List.indexedMap
            (\i value ->
                [ Just ( tupleIndexToNumericField i, value )
                , tupleIndexToWordyField i |> Maybe.map (\field -> ( field, value ))
                ]
                    |> List.filterMap identity
            )
        |> List.concat
        |> Dict.fromList


{-| Try to find the ID in the current Env, then in the parent, ... up to the root.
-}
interpretIdentifier : StmtMonad -> Interpreter Id Value
interpretIdentifier stmtMonad =
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
                                let
                                    justReturn =
                                        Outcome.succeed env value
                                in
                                case value of
                                    VIntrinsicCall intrinsic values ->
                                        interpretIntrinsicCallValues stmtMonad env ( intrinsic, values )

                                    -- TODO should we check and call VIntrinsicCalls recursively? Not completely sure about these:
                                    VList _ ->
                                        justReturn

                                    VTuple _ ->
                                        justReturn

                                    VRecord _ ->
                                        justReturn

                                    VConstructor _ ->
                                        justReturn

                                    VIntrinsic _ ->
                                        justReturn

                                    VClosure _ ->
                                        justReturn

                                    VIo _ ->
                                        justReturn

                                    -- Pretty sure about the rest:
                                    VInt _ ->
                                        justReturn

                                    VFloat _ ->
                                        justReturn

                                    VString _ ->
                                        justReturn

                                    VChar _ ->
                                        justReturn

                                    VBool _ ->
                                        justReturn

                                    VUnit ->
                                        justReturn

                                    VRecordGetter _ ->
                                        justReturn
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


interpretConstructor : StmtMonad -> Interpreter { id : Id, args : List Expr } Value
interpretConstructor stmtMonad =
    \env { id, args } ->
        Interpreter.traverse (interpretExpr stmtMonad) env args
            |> Outcome.map (\argValues -> VConstructor { id = id, args = argValues })


interpretIf : StmtMonad -> Interpreter { cond : Expr, then_ : Expr, else_ : Expr } Value
interpretIf stmtMonad =
    \env { cond, then_, else_ } ->
        Interpreter.do (interpretExpr stmtMonad env cond) <| \env1 cond_ ->
        case cond_ of
            VBool True ->
                interpretExpr stmtMonad env1 then_

            VBool False ->
                interpretExpr stmtMonad env1 else_

            _ ->
                Outcome.fail IfConditionNotBool


interpretCase : StmtMonad -> Interpreter { subject : Expr, branches : List CaseBranch } Value
interpretCase stmtMonad =
    \env { subject, branches } ->
        Interpreter.do (interpretExpr stmtMonad env subject) <| \env1 subject_ ->
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
        interpretCaseBranches stmtMonad env1 ( subject_, flatBranches )


interpretCaseBranches : StmtMonad -> Interpreter ( Value, List ( Pattern, Expr ) ) Value
interpretCaseBranches stmtMonad =
    \env ( subject, branches ) ->
        case branches of
            [] ->
                Outcome.fail NoCaseBranchMatched

            ( pattern, body ) :: rest ->
                Interpreter.do (interpretPattern stmtMonad env ( pattern, subject )) <| \env1 maybeAdditions ->
                case maybeAdditions of
                    Nothing ->
                        interpretCaseBranches stmtMonad env ( subject, rest )

                    Just additions ->
                        let
                            newEnv =
                                addToEnv additions env1
                        in
                        interpretExpr stmtMonad newEnv body


interpretList : StmtMonad -> Interpreter (List Expr) Value
interpretList stmtMonad =
    Interpreter.traverse (interpretExpr stmtMonad)
        |> Interpreter.map VList


interpretTuple : StmtMonad -> Interpreter (List Expr) Value
interpretTuple stmtMonad =
    Interpreter.traverse (interpretExpr stmtMonad)
        |> Interpreter.map VTuple


interpretRecord : StmtMonad -> Interpreter (List RecordExprContent) Value
interpretRecord stmtMonad =
    Interpreter.traverse (interpretRecordExprContent stmtMonad)
        |> Interpreter.map
            (\contents ->
                contents
                    |> List.concat
                    |> -- TODO which fields are we throwing away on collision - older or newer?
                       -- TODO should we throw an error instead? Perhaps in typechecker
                       Dict.fromList
                    |> VRecord
            )


interpretRecordExprContent : StmtMonad -> Interpreter RecordExprContent (List ( String, Value ))
interpretRecordExprContent stmtMonad =
    \env content ->
        case content of
            Field { field, expr } ->
                interpretExpr stmtMonad env expr
                    |> Outcome.map (\value -> [ ( field, value ) ])

            Pun field ->
                interpretExpr stmtMonad env (Identifier (Id.local field))
                    |> Outcome.map (\value -> [ ( field, value ) ])

            Spread id ->
                Interpreter.do (interpretExpr stmtMonad env (Identifier id)) <| \env1 value ->
                case value of
                    VRecord fields ->
                        Outcome.succeed env1 (Dict.toList fields)

                    VTuple values ->
                        Outcome.succeed env1 (Dict.toList (tupleToNumericRecord values))

                    _ ->
                        Outcome.fail SpreadingNonRecord


interpretModule : Interpreter { mod : ModuleModifier, name : String, decls : List Decl } ()
interpretModule =
    \env { mod, name, decls } ->
        -- TODO handle the modifier
        let
            envWithOpenModule : Maybe (Env Value)
            envWithOpenModule =
                env
                    |> Env.createModule name
                    |> Env.goInto [ name ]
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
        NeedsEffect0 (Effect.Println (Value.toShowString value)) <|
            \() -> Outcome.succeed env ()


interpretInspect : Interpreter Value ()
interpretInspect =
    \env value ->
        NeedsEffect0 (Effect.Println (Value.toInspectString value)) <|
            \() -> Outcome.succeed env ()


interpretReadFile : Interpreter Value (Maybe String)
interpretReadFile =
    \env arg ->
        case arg of
            VString filename ->
                NeedsEffectMaybeStr (Effect.ReadFileMaybe { filename = filename }) <| \maybeStr ->
                Outcome.succeed env maybeStr

            _ ->
                Outcome.fail <| UnexpectedArgument arg


interpretWriteFile : Interpreter Value Bool
interpretWriteFile =
    \env arg ->
        case arg of
            VRecord dict ->
                Maybe.map2
                    (\filename content ->
                        case ( filename, content ) of
                            ( VString filename_, VString content_ ) ->
                                NeedsEffectBool (Effect.WriteFileMaybe { filename = filename_, content = content_ }) <| \bool ->
                                Outcome.succeed env bool

                            _ ->
                                Outcome.fail <| UnexpectedArgument arg
                    )
                    (Dict.get "filename" dict)
                    (Dict.get "content" dict)
                    |> Maybe.withDefault (Outcome.fail <| UnexpectedArgument arg)

            _ ->
                Outcome.fail <| UnexpectedArgument arg
