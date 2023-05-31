module Interpreter exposing (interpretProgram)

import AST exposing (..)
import Effect exposing (Effect0, EffectStr)
import Env exposing (Env)
import Error exposing (InterpreterError(..))
import Id exposing (Id)
import Interpreter.Internal as Interpreter exposing (Interpreter)
import Interpreter.Outcome as Outcome exposing (Outcome(..))
import Intrinsic exposing (Intrinsic(..))
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
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


interpretBang : Interpreter Bang Value
interpretBang =
    \env bang ->
        case bang of
            BValue expr ->
                Debug.todo "branch 'BValue' not implemented"

            BCall r ->
                interpretBangCall env r


interpretBangCall : Interpreter { fn : Expr, args : List Expr } Value
interpretBangCall =
    \env { fn, args } ->
        interpretExpr env fn
            |> Interpreter.andThen
                (\env1 fnVal ->
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
                )


interpretLet : Interpreter { name : String, expr : Expr } ()
interpretLet =
    \env { name, expr } ->
        interpretExpr env expr
            |> Outcome.mapBoth
                (\env_ value ->
                    ( env |> Env.add name value
                    , ()
                    )
                )


interpretExpr : Interpreter Expr Value
interpretExpr =
    \env expr ->
        case expr of
            Int n ->
                Outcome.succeed env (VInt n)

            Identifier id ->
                interpretIdentifier env id

            RootIdentifier id ->
                interpretRootIdentifier env id

            _ ->
                Debug.todo <| "Unimplemented interpretExpr: " ++ Debug.toString expr


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


interpretModule : Interpreter { name : String, decls : List Decl } ()
interpretModule =
    \env { name, decls } ->
        let
            envWithOpenModule : Maybe Env
            envWithOpenModule =
                env
                    |> Env.addModule name
                    |> Env.open [ name ]
        in
        case envWithOpenModule of
            Nothing ->
                -- Should never happen
                Outcome.fail <| ExpectedModule name

            Just envWithOpenModule_ ->
                interpretProgram envWithOpenModule_ decls


interpretPrintln : Interpreter Value ()
interpretPrintln =
    \env value ->
        NeedsEffect0 (Effect.Println (Value.toString value)) <|
            \() -> Outcome.succeed env ()
