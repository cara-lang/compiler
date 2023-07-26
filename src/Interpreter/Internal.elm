module Interpreter.Internal exposing
    ( Interpreter
    , succeed, fail
    , map, andThen, do
    , traverse
    )

{-|

@docs Interpreter
@docs succeed, fail
@docs map, andThen, do
@docs traverse

-}

import Env exposing (Env)
import Error exposing (InterpreterError)
import Interpreter.Outcome as Outcome exposing (Outcome(..))
import Loc exposing (Loc)
import Value exposing (Value)


type alias Interpreter a b =
    Env Value -> a -> Outcome b


succeed : b -> Interpreter a b
succeed b =
    \env _ -> DoneInterpreting env b


fail : InterpreterError -> Interpreter a b
fail err =
    \_ _ -> FoundError ( Debug.todo "interpreter internal - fail - loc", err )


map : (b -> bb) -> Interpreter a b -> Interpreter a bb
map fn interpreter =
    \env a ->
        interpreter env a
            |> Outcome.map fn


{-| This could be:

    andThen : Interpreter b c -> Interpreter a b -> Interpreter a c
    andThen int2 int1 =
        \env a ->
            case int1 env a of
                DoneInterpreting env1 b ->
                    int2 env1 b

                FoundError err ->
                    FoundError err

                NeedsEffect0 eff k ->
                    NeedsEffect0 eff k
                        |> Outcome.andThen int2

                NeedsEffectStr eff k ->
                    NeedsEffectStr eff k
                        |> Outcome.andThen int2

                NeedsEffectMaybeStr eff k ->
                    NeedsEffectMaybeStr eff k
                        |> Outcome.andThen int2

But that leads to code like

    (interpretExpr
        |> Interpreter.andThen interpretPrintln
    )
        env1
        arg

So we'd rather have

    Interpreter.andThen : Interpreter b c -> Outcome b -> Outcome c

and have code like

    interpretExpr env1 arg
        |> Interpreter.andThen interpretPrintln

-}
andThen : Interpreter b c -> Outcome b -> Outcome c
andThen interpreter outcome =
    case outcome of
        DoneInterpreting env b ->
            interpreter env b

        FoundError err ->
            FoundError err

        NeedsEffect0 eff k ->
            NeedsEffect0 eff k
                |> Outcome.andThen interpreter

        NeedsEffectStr eff k ->
            NeedsEffectStr eff k
                |> Outcome.andThen interpreter

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff k
                |> Outcome.andThen interpreter

        NeedsEffectBool eff k ->
            NeedsEffectBool eff k
                |> Outcome.andThen interpreter


do : Outcome b -> Interpreter b c -> Outcome c
do outcome interpreter =
    andThen interpreter outcome


traverse : Interpreter a b -> Interpreter (List a) (List b)
traverse innerInterpreter =
    \env list ->
        let
            go : Env Value -> List b -> List a -> Outcome (List b)
            go env1 acc todo =
                case todo of
                    [] ->
                        DoneInterpreting env1 (List.reverse acc)

                    x :: xs ->
                        innerInterpreter env1 x
                            |> andThen (\env2 y -> go env2 (y :: acc) xs)
        in
        go env [] list
