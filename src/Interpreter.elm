module Interpreter exposing (interpretProgram)

import AST.Frontend as AST exposing (..)
import Basics.Extra as Basics
import BiDict
import Bitwise
import Common
import Console
import Debug.Extra
import Dict exposing (Dict)
import Effect
import Env exposing (Env)
import Error exposing (InterpreterError(..))
import Id exposing (Id)
import Interpreter.Internal as Interpreter exposing (Interpreter)
import Interpreter.Outcome as Outcome exposing (Outcome(..))
import Intrinsic exposing (Intrinsic(..))
import List.Extra
import Maybe.Extra
import NonemptyList exposing (NonemptyList)
import Operator exposing (BinaryOp(..), Operator(..), UnaryOp(..))
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
    \env program ->
        Interpreter.do (Interpreter.traverse interpretDecl env program) <| \env2 _ ->
        case Env.get Id.main_ env2 of
            Nothing ->
                Outcome.succeed env2 ()

            Just ((VClosure _) as main_) ->
                interpretCallVal ToplevelIO env2 ( main_, [] )
                    |> Outcome.map (\_ -> ())

            Just _ ->
                Outcome.fail MainIsNotFunction


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

            DIntrinsicType r ->
                interpretIntrinsicTypeDecl env r

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
                let
                    _ =
                        Debug.log "The impossible happened (interpretExtendModule)" ()
                in
                Outcome.fail <| ExpectedModule (String.join "." module_)

            Just envWithOpenModule_ ->
                interpretProgram envWithOpenModule_ decls
                    |> Basics.doNTimes (List.length module_) (Outcome.attemptMapEnv Env.goUp ExpectedParent)


interpretUnitTest : Interpreter { name : Maybe String, expr : Expr } ()
interpretUnitTest =
    \env { name, expr } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretUnitTest") (Maybe.withDefault "" name)
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretParameterizedTest : Interpreter { name : Maybe String, table : List Expr, args : List Pattern, expr : Expr } ()
interpretParameterizedTest =
    \env { name, table, args, expr } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretParameterizedTest") (Maybe.withDefault "" name)
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretPropertyTypeTest : Interpreter { name : Maybe String, types : Type, args : List Pattern, expr : Expr } ()
interpretPropertyTypeTest =
    \env { name, types, args, expr } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretPropertyTypeTest") (Maybe.withDefault "" name)
        in
        -- TODO actually do something
        Outcome.succeed env ()


interpretPropertyGenTest : Interpreter { name : Maybe String, gens : Expr, args : List Pattern, expr : Expr } ()
interpretPropertyGenTest =
    \env { name, gens, args, expr } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretPropertyGenTest") (Maybe.withDefault "" name)
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


interpretFunctionDef :
    Interpreter
        { mod : LetModifier
        , name : String
        , branches : NonemptyList FunctionBranch
        }
        ()
interpretFunctionDef =
    \env ({ mod, name } as r) ->
        -- TODO interpret the modifier
        let
            { args, body } =
                r
                    |> AST.functionDefToSingleFunction
                    |> AST.makeRecursive name

            envWithFn : Env Value
            envWithFn =
                env
                    |> Env.add name
                        (VClosure
                            -- TODO we're throwing away the fndef types here. Have we processed them elsewhere?
                            { args = args
                            , body = body
                            , env = env
                            }
                        )
        in
        Outcome.succeed envWithFn ()


interpretBinaryOpDef :
    Interpreter
        { op : BinaryOp
        , left : ( Pattern, Maybe Type )
        , right : ( Pattern, Maybe Type )
        , body : Expr
        }
        ()
interpretBinaryOpDef =
    \env { op, left, right, body } ->
        let
            newEnv =
                Env.addBinaryOp (Operator.binaryOpToString op)
                    (VClosure
                        -- TODO we're throwing away the opdef arg types here. Have we processed them elsewhere?
                        { args = List.map Tuple.first [ left, right ]
                        , body = body
                        , env = env
                        }
                    )
                    env
        in
        -- TODO make type annotations do something
        Outcome.succeed newEnv ()


interpretUnaryOpDef :
    Interpreter
        { op : UnaryOp
        , arg : ( Pattern, Maybe Type )
        , body : Expr
        }
        ()
interpretUnaryOpDef =
    \env { op, arg, body } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretUnaryOpDef") ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretValueAnnotation : Interpreter { mod : LetModifier, name : String, type_ : Type } ()
interpretValueAnnotation =
    \env { mod, name, type_ } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretValueAnnotation") name
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretBinaryOpAnnotation : Interpreter { mod : LetModifier, op : BinaryOp, left : Type, right : Type, ret : Type } ()
interpretBinaryOpAnnotation =
    \env { mod, op, left, right, ret } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretBinaryOpAnnotation") ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUnaryOpAnnotation : Interpreter { mod : LetModifier, op : UnaryOp, arg : Type, ret : Type } ()
interpretUnaryOpAnnotation =
    \env { mod, op, arg, ret } ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretUnaryOpAnnotation") ()
        in
        -- TODO make type annotations do something
        Outcome.succeed env ()


interpretUseModule : Interpreter Id ()
interpretUseModule =
    \env moduleId ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretUseModule") ()

            modulePath =
                moduleId.qualifiers ++ [ moduleId.name ]
        in
        Outcome.succeed env ()


interpretIntrinsicTypeDecl :
    Interpreter
        { name : String
        , vars : List String
        }
        ()
interpretIntrinsicTypeDecl =
    \env r ->
        let
            _ =
                Debug.log (Console.dark "TODO interpretIntrinsicTypeDecl") ()
        in
        Outcome.succeed env ()


interpretTypeDecl :
    Interpreter
        { mod : TypeModifier
        , name : String
        , vars : List String
        , constructors : List TypeConstructor
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
                            if List.isEmpty c.args then
                                AddValue c.name <|
                                    VConstructor
                                        { rootId = Env.localId env c.name
                                        , args = []
                                        }

                            else
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
                                                { id = Env.localId env c.name
                                                , args = List.map (Identifier << Id.simple) names
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
            --let
            --    _ =
            --        Debug.log "adding to env" ( name, Value.toInspectString value )
            --in
            Env.add name value env

        AddBinaryOp op value ->
            Env.addBinaryOp (Operator.binaryOpToString op) value env

        AddUnaryOp op value ->
            Env.addUnaryOp (Operator.unaryOpToString op) value env

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
                Debug.log (Console.dark "TODO interpretTypeAlias") name
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

            PBool bool ->
                Outcome.succeed env <|
                    case value of
                        VBool v ->
                            if bool == v then
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
                        Interpreter.do (interpretRootIdentifier stmtMonad env id) <| \env1 found ->
                        let
                            handleVConstructor : Interpreter { rootId : Id, args : List Value } (Maybe PatternAddition)
                            handleVConstructor =
                                \envX vc ->
                                    if vc.rootId == r.rootId then
                                        let
                                            pairs : List ( Pattern, Value )
                                            pairs =
                                                List.map2 Tuple.pair args r.args
                                        in
                                        Interpreter.do (Interpreter.traverse (interpretPattern stmtMonad) envX pairs) <| \envX1 maybeAdditions ->
                                        case Maybe.Extra.combine maybeAdditions of
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
                        interpretPatternList { canHaveSpreads = True } stmtMonad env ( ps, vs )

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
                Debug.Extra.todo1 "interpret as-pattern" (AST.patternToString pattern)

            POr l r ->
                Interpreter.do (interpretPattern stmtMonad env ( l, value )) <| \env1 maybeAddition ->
                case maybeAddition of
                    Nothing ->
                        interpretPattern stmtMonad env1 ( r, value )

                    Just _ ->
                        Outcome.succeed env1 maybeAddition


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
            case Maybe.Extra.combine maybeAdditions of
                Nothing ->
                    Outcome.succeed env1 Nothing

                Just additions ->
                    Outcome.succeed env1 (Just (ManyAdditions additions))


interpretPatternList : { canHaveSpreads : Bool } -> StmtMonad -> Interpreter ( List Pattern, List Value ) (Maybe PatternAddition)
interpretPatternList { canHaveSpreads } stmtMonad =
    \env ( patterns, values ) ->
        let
            spreadsCount =
                List.length (List.filter AST.isSpreadPattern patterns)

            patternsCount =
                List.length patterns

            valuesCount =
                List.length values
        in
        case ( spreadsCount, canHaveSpreads ) of
            ( 0, _ ) ->
                -- [],   []    -> Just AddNothing
                -- [x],  [1]   -> x=1
                -- [x,y],[1,2] -> x=1, y=2
                if patternsCount == valuesCount then
                    Interpreter.do (Interpreter.traverse (interpretPattern stmtMonad) env (List.map2 Tuple.pair patterns values)) <| \env1 maybeAdditions ->
                    case Maybe.Extra.combine maybeAdditions of
                        Nothing ->
                            Outcome.fail PatternMismatch

                        Just additions ->
                            Outcome.succeed env1 (Just (ManyAdditions additions))

                else
                    Outcome.succeed env Nothing

            ( 1, True ) ->
                case patterns of
                    (PSpread spread) :: restPatterns ->
                        -- [...xs,    x], [1,2,3,4] --> xs=[1,2,3], x=4
                        -- [...xs, x, y], [1,2,3,4] --> xs=[1,2],   x=3, y=4
                        let
                            ( takenBySpread, takenByRest ) =
                                List.Extra.splitAt (valuesCount - patternsCount + 1 {- n of spread items -}) values
                        in
                        Interpreter.do (interpretPatternList { canHaveSpreads = False } stmtMonad env ( restPatterns, takenByRest )) <| \env1 maybeAddition ->
                        case maybeAddition of
                            Nothing ->
                                Outcome.fail PatternMismatch

                            Just addition ->
                                case spread of
                                    Just spreadName ->
                                        let
                                            newAddition =
                                                ManyAdditions
                                                    -- TODO in case somebody does [...x,x], this order will become important. Let's pin this down with a test
                                                    [ addition
                                                    , AddValue spreadName (VList takenBySpread)
                                                    ]
                                        in
                                        Outcome.succeed env1 (Just newAddition)

                                    Nothing ->
                                        Outcome.succeed env1 maybeAddition

                    firstPattern :: restPatterns ->
                        case List.reverse patterns of
                            (PSpread spread) :: revButlastPatterns ->
                                -- [x,    ...xs], [1,2,3,4] --> x=1,      xs=[2,3,4]
                                -- [x, y, ...xs], [1,2,3,4] --> x=1, y=2, xs=[3,4]
                                let
                                    ( takenByRest, takenBySpread ) =
                                        List.Extra.splitAt (patternsCount - 1 {- n of non-spread items -}) values
                                in
                                Interpreter.do (interpretPatternList { canHaveSpreads = False } stmtMonad env ( List.reverse revButlastPatterns, takenByRest )) <| \env1 maybeAddition ->
                                case maybeAddition of
                                    Nothing ->
                                        Outcome.fail PatternMismatch

                                    Just addition ->
                                        case spread of
                                            Just spreadName ->
                                                let
                                                    newAddition =
                                                        ManyAdditions
                                                            -- TODO in case somebody does [x,...x], this order will become important. Let's pin this down with a test
                                                            [ addition
                                                            , AddValue spreadName (VList takenBySpread)
                                                            ]
                                                in
                                                Outcome.succeed env1 (Just newAddition)

                                            Nothing ->
                                                Outcome.succeed env1 maybeAddition

                            lastPattern :: revButlastPatterns ->
                                {- [x, ...middle, y], [1,2,3,4] --> x=1, xs=[2,3], y=4

                                   Because we'd have hard time destructuring the Spread
                                   that's somewhere in the middle, we rather just slice
                                   one of the values off the end and then try
                                   interpretPatternList again. Eventually the spread
                                   will be at the beginning of the list and some other
                                   case..of branch will fire.
                                -}
                                case List.reverse values of
                                    [] ->
                                        Outcome.fail PatternMismatch

                                    lastValue :: revRestValues ->
                                        Interpreter.do (interpretPattern stmtMonad env ( lastPattern, lastValue )) <| \env1 maybeAddition1 ->
                                        Interpreter.do (interpretPatternList { canHaveSpreads = True } stmtMonad env1 ( List.reverse revButlastPatterns, List.reverse revRestValues )) <| \env2 maybeAddition2 ->
                                        case Maybe.Extra.combine [ maybeAddition1, maybeAddition2 ] of
                                            Nothing ->
                                                Outcome.fail PatternMismatch

                                            Just additions ->
                                                Outcome.succeed env2 (Just (ManyAdditions additions))

                            [] ->
                                Debug.todo "This shouldn't be possible - compiler bug. We've just seen there's one PSpread somewhere in the list, so the list can't be empty."

                    [] ->
                        Debug.todo "This shouldn't be possible - compiler bug. We've just seen there's one PSpread somewhere in the list, so the list can't be empty."

            ( _, True ) ->
                Outcome.fail MultipleSpreadPatterns

            ( _, False ) ->
                Outcome.fail PatternMismatch


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
                interpretRootIdentifier stmtMonad env id

            OpIdentifier op ->
                interpretOpIdentifier env op

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
                        interpretRecordGetDict env ( field, tupleToNumericRecord values )

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
                                    List.Extra.splitAt argsLength argVals
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
                        case Maybe.Extra.combine maybeAdditions of
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
                        case Maybe.Extra.combine maybeAdditions of
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
        Outcome.succeed env <|
            VClosure { args = args, body = body, env = env }


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

                    SFunctionDef r ->
                        -- TODO do something about the mod
                        let
                            { args, body } =
                                AST.functionDefToSingleFunction r
                        in
                        addLayer
                            (SLet
                                { lhs = PVar r.name
                                , expr =
                                    Lambda
                                        { -- TODO we're throwing away the fndef arg types here
                                          args = args
                                        , body = body
                                        }
                                , mod = LetNoModifier
                                , type_ = Nothing
                                }
                            )
                            innerExpr

                    SUnaryOperatorDef { op, arg, body } ->
                        addLayer
                            (SLet
                                { lhs = PUnaryOpDef op
                                , expr =
                                    Lambda
                                        { -- TODO we're throwing away the opdef arg types here
                                          args = [ Tuple.first arg ]
                                        , body = body
                                        }
                                , mod = LetNoModifier
                                , type_ = Nothing
                                }
                            )
                            innerExpr

                    SBinaryOperatorDef { op, left, right, body } ->
                        addLayer
                            (SLet
                                { lhs = PBinaryOpDef op
                                , expr =
                                    Lambda
                                        { -- TODO we're throwing away the opdef arg types here
                                          args = List.map Tuple.first [ left, right ]
                                        , body = body
                                        }
                                , mod = LetNoModifier
                                , type_ = Nothing
                                }
                            )
                            innerExpr

                    SValueAnnotation r ->
                        let
                            _ =
                                Debug.log "TODO interpretEffectBlock - SValueAnnotation" ()
                        in
                        innerExpr

                    SUnaryOperatorAnnotation r ->
                        let
                            _ =
                                Debug.log "TODO interpretEffectBlock - SUnaryOperatorAnnotation" ()
                        in
                        innerExpr

                    SBinaryOperatorAnnotation r ->
                        let
                            _ =
                                Debug.log "TODO interpretEffectBlock - SBinaryOperatorAnnotation" ()
                        in
                        innerExpr

                    SUseModule m ->
                        let
                            _ =
                                Debug.log "TODO interpretEffectBlock - SUseModule" ()
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
                case Env.getUnaryOp (Operator.unaryOpToString op) env of
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
        let
            eager () =
                Interpreter.do (interpretExpr stmtMonad env left) <| \env1 leftVal ->
                Interpreter.do (interpretExpr stmtMonad env1 right) <| \env2 rightVal ->
                interpretBinaryOpCallVal stmtMonad env2 ( leftVal, op, rightVal )

            shortCircuit wantedValue =
                Interpreter.do (interpretExpr stmtMonad env left) <| \env1 leftVal ->
                if leftVal == wantedValue then
                    Outcome.succeed env1 leftVal

                else
                    Interpreter.do (interpretExpr stmtMonad env1 right) <| \env2 rightVal ->
                    interpretBinaryOpCallVal stmtMonad env2 ( leftVal, op, rightVal )
        in
        case op of
            AndBool ->
                shortCircuit (VBool False)

            OrBool ->
                shortCircuit (VBool True)

            _ ->
                eager ()


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
            ( VInt a, Modulo, VInt b ) ->
                Outcome.succeed env <| VInt (a |> modBy b)

            ( VFloat a, Modulo, VInt b ) ->
                let
                    integer =
                        floor a

                    result =
                        toFloat (integer |> modBy b) + a - toFloat integer
                in
                Outcome.succeed env <| VFloat result

            ( VFloat a, Modulo, VFloat b ) ->
                Outcome.succeed env <| VFloat (a - b * toFloat (floor (a / b)))

            -- Pow: ints, floats
            ( VInt a, Power, VInt b ) ->
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
                case Env.getBinaryOp (Operator.binaryOpToString op) env of
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
                interpretTupleGetDict env1 ( field, tupleToNumericRecord values )

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
                Outcome.fail (AccessingMissingTupleElement field)

            Just content ->
                Outcome.succeed env content


{-| Useful for spreads
-}
tupleToNumericRecord : List Value -> Dict String Value
tupleToNumericRecord values =
    values
        |> List.indexedMap (\i value -> ( Common.tupleIndexToNumericField i, value ))
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
interpretRootIdentifier : StmtMonad -> Interpreter Id Value
interpretRootIdentifier stmtMonad =
    \env id ->
        case Env.get id (Zipper.root env) of
            Nothing ->
                Outcome.fail <| RootVarNotFound id

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


interpretConstructor : StmtMonad -> Interpreter { id : Id, args : List Expr } Value
interpretConstructor stmtMonad =
    \env { id, args } ->
        Interpreter.traverse (interpretExpr stmtMonad) env args
            |> Outcome.map
                (\argValues ->
                    VConstructor
                        { rootId = id
                        , args = argValues
                        }
                )


interpretOpIdentifier : Interpreter Operator Value
interpretOpIdentifier =
    \env op ->
        {- TODO this will probably need the type information on how
           this is used (is it the unary negation or the binary minus?
           etc.)

           Until then, we'll do something wrong to not block the e2e test suite.
        -}
        Outcome.succeed env (VString "op identifiers (used as functions) not implemented yet, waits for type inference")


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
        interpretCaseBranches stmtMonad env1 ( subject_, branches )


interpretCaseBranches : StmtMonad -> Interpreter ( Value, List CaseBranch ) Value
interpretCaseBranches stmtMonad =
    \env ( subject, branches ) ->
        case branches of
            [] ->
                Outcome.fail NoCaseBranchMatched

            branch :: rest ->
                Interpreter.do (interpretPattern stmtMonad env ( branch.pattern, subject )) <| \env1 maybeAdditions ->
                case maybeAdditions of
                    Nothing ->
                        interpretCaseBranches stmtMonad env ( subject, rest )

                    Just additions ->
                        let
                            newEnv =
                                addToEnv additions env1
                        in
                        interpretExpr stmtMonad newEnv branch.body


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
                interpretExpr stmtMonad env (Identifier (Env.localId env field))
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
                let
                    _ =
                        Debug.log "The impossible happened (interpretModule)" ()
                in
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


envToString : Env Value -> String
envToString env =
    Env.toString { valueToString = Value.withClosures Value.toInspectString } env


envToString_ : Env Value -> String
envToString_ env =
    Env.toString { valueToString = Value.toInspectString } env
