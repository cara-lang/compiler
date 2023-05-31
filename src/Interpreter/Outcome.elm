module Interpreter.Outcome exposing
    ( Outcome(..)
    , succeed, fail
    , map, mapBoth, andThen
    )

{-|

@docs Outcome
@docs succeed, fail
@docs map, mapBoth, andThen

-}

import Effect exposing (Effect0, EffectStr)
import Env exposing (Env)
import Error exposing (InterpreterError)


type Outcome a
    = DoneInterpreting Env a
    | NeedsEffect0 Effect0 (() -> Outcome a)
    | NeedsEffectStr EffectStr (String -> Outcome a)
    | FoundError InterpreterError


succeed : Env -> a -> Outcome a
succeed env a =
    DoneInterpreting env a


fail : InterpreterError -> Outcome a
fail err =
    FoundError err


map : (a -> b) -> Outcome a -> Outcome b
map fn outcome =
    case outcome of
        DoneInterpreting env a ->
            DoneInterpreting env (fn a)

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> map fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> map fn)

        FoundError err ->
            FoundError err


mapBoth : (Env -> a -> ( Env, b )) -> Outcome a -> Outcome b
mapBoth fn outcome =
    case outcome of
        DoneInterpreting env a ->
            let
                ( env_, b ) =
                    fn env a
            in
            DoneInterpreting env_ b

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> mapBoth fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> mapBoth fn)

        FoundError err ->
            FoundError err


andThen : (Env -> a -> Outcome b) -> Outcome a -> Outcome b
andThen fn outcome1 =
    case outcome1 of
        DoneInterpreting env1 a ->
            case fn env1 a of
                DoneInterpreting env2 b ->
                    DoneInterpreting env2 b

                NeedsEffect0 eff k ->
                    NeedsEffect0 eff k

                NeedsEffectStr eff k ->
                    NeedsEffectStr eff k

                FoundError err ->
                    FoundError err

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> andThen fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> andThen fn)

        FoundError err ->
            FoundError err
