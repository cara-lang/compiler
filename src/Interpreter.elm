module Interpreter exposing (interpretProgram)

import AST exposing (..)
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

            DValueAnnotation r ->
                interpretValueAnnotation env r

            DTypeDecl r ->
                interpretTypeDecl env r

            _ ->
                Debug.todo <| "Unimplemented interpretDecl: " ++ Debug.toString decl


interpretStatement : Interpreter Stmt ()
interpretStatement =
    \env stmt ->
        case stmt of
            SLet r ->
                interpretLet env r

            SLetBang _ ->
                Debug.todo "branch 'SLetBang' not implemented"

            SBang bang ->
                interpretBang env bang
                    -- Throw the result away
                    |> Outcome.map (\_ -> ())


interpretValueAnnotation : Interpreter { name : String, type_ : Type } ()
interpretValueAnnotation =
    \env { name, type_ } ->
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


interpretBang : Interpreter Bang Value
interpretBang =
    \env bang ->
        case bang of
            BValue _ ->
                Debug.todo "branch 'BValue' not implemented"

            BCall r ->
                interpretBangCall env r


interpretBangCall : Interpreter { fn : Expr, args : List Expr } Value
interpretBangCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <|
            \env1 fnVal ->
                case fnVal of
                    VIntrinsic IoPrintln ->
                        case args of
                            [ arg ] ->
                                interpretExpr env1 arg
                                    |> Interpreter.andThen interpretPrintln
                                    |> Outcome.map (\() -> VUnit)

                            _ ->
                                Outcome.fail UnexpectedArity

                    _ ->
                        Debug.todo "Unsupported Value node in the `fn` position of a BangCall"


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


{-| Returns (NEW) env additions, instead of the whole env.
Nothing is returned if the pattern doesn't match the expr.
-}
interpretPattern : Interpreter ( Pattern, Value ) (Maybe (EnvDict Value))
interpretPattern =
    \env ( pattern, value ) ->
        case pattern of
            PVar var ->
                Outcome.succeed env
                    (Just
                        (EnvDict.singleton
                            { qualifiers = [], name = var }
                            value
                        )
                    )

            _ ->
                Debug.todo "interpret pattern - other"


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

            Unit ->
                Outcome.succeed env VUnit

            List xs ->
                interpretList env xs

            Tuple xs ->
                interpretTuple env xs

            UnaryOp op e ->
                interpretUnaryOp env ( op, e )

            BinaryOp left op right ->
                interpretBinaryOp env ( left, op, right )

            RecordGet r ->
                interpretRecordGet env r

            RecordGetter field ->
                Outcome.succeed env (VRecordGetter field)

            Call r ->
                interpretCall env r

            _ ->
                Debug.todo <| "Unimplemented interpretExpr: " ++ Debug.toString expr


interpretCall : Interpreter { fn : Expr, args : List Expr } Value
interpretCall =
    \env { fn, args } ->
        interpretExpr env fn
            |> Outcome.andThen
                (\env1 fnVal ->
                    Interpreter.traverse interpretExpr env1 args
                        |> Outcome.andThen
                            (\env2 argVals ->
                                case ( fnVal, argVals ) of
                                    ( VRecordGetter field, [ VTuple values ] ) ->
                                        interpretRecordGetTuple env2 ( values, field )

                                    _ ->
                                        Debug.todo <| "interpretCall interpreted: " ++ Debug.toString ( fnVal, argVals )
                            )
                )


interpretUnaryOp : Interpreter ( UnaryOp, Expr ) Value
interpretUnaryOp =
    \env ( op, expr ) ->
        interpretExpr env expr
            |> Outcome.andThen (\env1 val -> interpretUnaryOpVal env1 ( op, val ))


interpretUnaryOpVal : Interpreter ( UnaryOp, Value ) Value
interpretUnaryOpVal =
    \env ( op, val ) ->
        case ( op, val ) of
            ( NegateNum, VInt n ) ->
                Outcome.succeed env <| VInt (negate n)

            ( NegateNum, VFloat n ) ->
                Outcome.succeed env <| VFloat (negate n)

            _ ->
                Debug.todo <| "Unimplemented interpretUnaryOpVal: " ++ Debug.toString ( op, val )


interpretBinaryOp : Interpreter ( Expr, BinaryOp, Expr ) Value
interpretBinaryOp =
    \env ( left, op, right ) ->
        interpretExpr env left
            |> Outcome.andThen
                (\env1 leftVal ->
                    interpretExpr env1 right
                        |> Outcome.andThen
                            (\env2 rightVal ->
                                interpretBinaryOpVal env2 ( leftVal, op, rightVal )
                            )
                )


interpretBinaryOpVal : Interpreter ( Value, BinaryOp, Value ) Value
interpretBinaryOpVal =
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

            _ ->
                Debug.todo <| "Unimplemented interpretBinaryOp: " ++ Debug.toString ( left, op, right )


interpretRecordGet : Interpreter { record : Expr, field : String } Value
interpretRecordGet =
    \env { record, field } ->
        interpretExpr env record
            |> Outcome.andThen
                (\env1 recordVal ->
                    case recordVal of
                        VTuple values ->
                            interpretRecordGetTuple env ( values, field )

                        _ ->
                            Debug.todo <| "Unimplemented interpretRecordGet: " ++ Debug.toString recordVal
                )


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


interpretList : Interpreter (List Expr) Value
interpretList =
    Interpreter.traverse interpretExpr
        |> Interpreter.map VList


interpretTuple : Interpreter (List Expr) Value
interpretTuple =
    Interpreter.traverse interpretExpr
        |> Interpreter.map VTuple


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
