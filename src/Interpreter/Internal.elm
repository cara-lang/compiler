module Interpreter.Internal exposing
    ( Interpreter
    , succeed, fail
    , map, contramap, andThen
    , traverse
    )

{-|

@docs Interpreter
@docs succeed, fail
@docs map, contramap, andThen
@docs traverse

-}

import Env exposing (Env)
import Error exposing (InterpreterError)
import Interpreter.Outcome as Outcome exposing (Outcome(..))


type alias Interpreter a b =
    Env -> a -> Outcome b


succeed : b -> Interpreter a b
succeed b =
    \env _ -> DoneInterpreting env b


fail : InterpreterError -> Interpreter a b
fail err =
    \_ _ -> FoundError err


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


contramap : (aa -> a) -> Interpreter a b -> Interpreter aa b
contramap fn interpreter =
    \env aa ->
        interpreter env (fn aa)


traverse : Interpreter a b -> Interpreter (List a) (List b)
traverse innerInterpreter =
    \env list ->
        let
            go : Env -> List b -> List a -> Outcome (List b)
            go env1 acc todo =
                case todo of
                    [] ->
                        DoneInterpreting env1 (List.reverse acc)

                    x :: xs ->
                        innerInterpreter env1 x
                            |> andThen (\env2 y -> go env2 (y :: acc) xs)
        in
        go env [] list
