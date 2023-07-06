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

            DFunction r ->
                interpretFunction env r

            DValueAnnotation r ->
                interpretValueAnnotation env r

            DType r ->
                interpretTypeDecl env r

            DTypeAlias r ->
                interpretTypeAlias env r

            DUnitTest r ->
                interpretUnitTest env r

            DParameterizedTest r ->
                interpretParameterizedTest env r

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


interpretParameterizedTest : Interpreter { name : Maybe String, table : List Expr, args : List Argument, expr : Expr } ()
interpretParameterizedTest =
    \env { name, table, args, expr } ->
        let
            _ =
                Debug.log "TODO do something in interpretParameterizedTest" ( name, table, ( args, expr ) )
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


interpretFunction : Interpreter { mod : FunctionModifier, name : String, args : List Argument, retType : Maybe Type, body : Expr } ()
interpretFunction =
    \env { mod, name, args, retType, body } ->
        -- TODO interpret the modifier
        -- TODO interpret the retType
        -- TODO somehow group related function declarations together and make one case..of from them
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


interpretValueAnnotation : Interpreter { name : String, type_ : Type } ()
interpretValueAnnotation =
    \env { name, type_ } ->
        let
            _ =
                Debug.log "TODO do something in interpretValueAnnotation" ( name, type_ )
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
                                { args = List.map (\name -> { pattern = PVar name, type_ = Nothing }) names
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
            BValue _ ->
                Debug.todo "branch 'BValue' not implemented"

            BCall r ->
                interpretBangCall env r


interpretBangCall : Interpreter { fn : Expr, args : List Expr } Value
interpretBangCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <| \env1 fnVal ->
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

                    _ ->
                        Debug.todo <| "interpret pattern {..} - other: " ++ Debug.toString value

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
                interpretUnaryOp env ( op, e )

            BinaryOp left op right ->
                interpretBinaryOp env ( left, op, right )

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

            _ ->
                Debug.todo <| "Unimplemented interpretExpr: " ++ Debug.toString expr


interpretCall : Interpreter { fn : Expr, args : List Expr } Value
interpretCall =
    \env { fn, args } ->
        Interpreter.do (interpretExpr env fn) <| \env1 fnVal ->
        Interpreter.do (Interpreter.traverse interpretExpr env1 args) <| \env2 argVals ->
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
                                -- TODO Dropping the argument `type_` - is that correct?
                                List.map2 (\arg val -> ( arg.pattern, val ))
                                    r.args
                                    argVals
                        in
                        Interpreter.do (Interpreter.traverse interpretPattern env2 pairs) <| \env3 maybeDicts ->
                        case Maybe.combine maybeDicts of
                            Nothing ->
                                Outcome.fail PatternMismatch

                            Just dicts ->
                                Interpreter.do (interpretExpr (List.foldl Env.addDict r.env dicts) r.body) <| \_ callResult ->
                                Outcome.succeed env3 callResult

            _ ->
                Debug.todo <| "interpretCall interpreted: " ++ Debug.toString ( fnVal, argVals )


interpretLambda : Interpreter { args : List Argument, body : Expr } Value
interpretLambda =
    \env { args, body } ->
        Outcome.succeed env <| VClosure { args = args, body = body, env = env }


interpretBlock : Interpreter { letStmts : List LetStmt, ret : Expr } Value
interpretBlock =
    \env { letStmts, ret } ->
        Interpreter.do (Interpreter.traverse interpretLet env letStmts) <| \env1 _ ->
        interpretExpr env1 ret


interpretUnaryOp : Interpreter ( UnaryOp, Expr ) Value
interpretUnaryOp =
    \env ( op, expr ) ->
        Interpreter.do (interpretExpr env expr) <| \env1 val ->
        interpretUnaryOpVal env1 ( op, val )


interpretUnaryOpVal : Interpreter ( UnaryOp, Value ) Value
interpretUnaryOpVal =
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


interpretBinaryOp : Interpreter ( Expr, BinaryOp, Expr ) Value
interpretBinaryOp =
    \env ( left, op, right ) ->
        Interpreter.do (interpretExpr env left) <| \env1 leftVal ->
        Interpreter.do (interpretExpr env1 right) <| \env2 rightVal ->
        interpretBinaryOpVal env2 ( leftVal, op, rightVal )


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
        Interpreter.do (interpretExpr env record) <| \env1 recordVal ->
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
        Interpreter.do (interpretExpr env cond) <| \env1 cond_ ->
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
        Interpreter.do (interpretExpr env subject) <| \env1 subject_ ->
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
                Interpreter.do (interpretPattern env ( pattern, subject )) <| \env1 maybeAdditions ->
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
