module Interpreter exposing
    ( Outcome(..)
    , interpret
    )

import AST exposing (AST(..), Id)
import Effect exposing (Effect)
import Env exposing (Env)
import Error exposing (InterpreterError(..))
import Tree.Zipper as Zipper exposing (Zipper)
import Value exposing (Value(..))


type Outcome
    = DoneInterpreting Env (Zipper AST) Value
    | NeedsEffect Env (Zipper AST) Effect
    | FoundError InterpreterError


interpret : Env -> Zipper AST -> Outcome
interpret env ast =
    case Zipper.label ast of
        Program _ ->
            interpretProgram env ast

        Let { name } ->
            interpretLet name env ast

        Int n ->
            done env ast (VInt n)

        Module { name } ->
            interpretModule name env ast

        Println ->
            interpretPrintln env ast

        Var r ->
            interpretVar r env ast

        RootVar r ->
            interpretRootVar r env ast


done : Env -> Zipper AST -> Value -> Outcome
done env ast value =
    case removeCurrent ast of
        Nothing ->
            DoneInterpreting env ast value

        Just newAst ->
            DoneInterpreting env newAst value


removeCurrent : Zipper AST -> Maybe (Zipper AST)
removeCurrent ast =
    Zipper.removeTree ast


interpretProgram : Env -> Zipper AST -> Outcome
interpretProgram env ast =
    case Zipper.firstChild ast of
        Nothing ->
            done env ast VUnit

        Just child ->
            case interpret env child of
                DoneInterpreting newEnv newAst _ ->
                    interpretProgram newEnv newAst

                other ->
                    other


interpretLet : String -> Env -> Zipper AST -> Outcome
interpretLet name env ast =
    case Zipper.firstChild ast of
        Nothing ->
            FoundError (ExpectedChildNode "let")

        Just child ->
            case interpret env child of
                DoneInterpreting newEnv newAst val ->
                    done (Env.add name val newEnv) newAst VUnit

                other ->
                    other


interpretModule : String -> Env -> Zipper AST -> Outcome
interpretModule name env ast =
    if Zipper.firstChild ast == Nothing then
        done env ast VUnit

    else
        let
            envWithOpenModule =
                env
                    |> Env.addModule name
                    |> Env.open [ name ]
        in
        case envWithOpenModule of
            Nothing ->
                -- Should never happen
                FoundError (ExpectedModule name)

            Just envWithOpenModule_ ->
                case interpretProgram envWithOpenModule_ ast of
                    DoneInterpreting envAfterChildren _ _ ->
                        done envAfterChildren ast VUnit

                    NeedsEffect newEnv newAst eff ->
                        case Zipper.parent newEnv of
                            Nothing ->
                                FoundError ExpectedParent

                            Just parentEnv ->
                                NeedsEffect parentEnv newAst eff

                    other ->
                        other


interpretPrintln : Env -> Zipper AST -> Outcome
interpretPrintln env ast =
    case Zipper.firstChild ast of
        Nothing ->
            NeedsEffect env ast (Effect.Println "")

        Just child ->
            case interpret env child of
                DoneInterpreting newEnv _ val ->
                    case removeCurrent ast of
                        Nothing ->
                            NeedsEffect newEnv ast (Effect.Println (Value.toString val))

                        Just newAst ->
                            NeedsEffect newEnv newAst (Effect.Println (Value.toString val))

                other ->
                    other


interpretVar : Id -> Env -> Zipper AST -> Outcome
interpretVar id env ast =
    let
        go env_ =
            case env_ of
                Nothing ->
                    FoundError (VarNotFound id)

                Just env__ ->
                    case Env.get id env__ of
                        Nothing ->
                            go (Zipper.parent env__)

                        Just value ->
                            done env ast value
    in
    go (Just env)


interpretRootVar : Id -> Env -> Zipper AST -> Outcome
interpretRootVar id env ast =
    case Env.get id (Zipper.root env) of
        Nothing ->
            FoundError (RootVarNotFound id)

        Just value ->
            done env ast value
