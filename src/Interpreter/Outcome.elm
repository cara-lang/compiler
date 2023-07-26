module Interpreter.Outcome exposing
    ( Outcome(..)
    , succeed, fail
    , map, mapBoth, mapEnv, attemptMapEnv
    , andThen, onError
    )

{-|

@docs Outcome
@docs succeed, fail
@docs map, mapBoth, mapEnv, attemptMapEnv
@docs andThen, onError

-}

import Effect exposing (Effect0, EffectBool, EffectMaybeStr, EffectStr)
import Env exposing (Env)
import Error exposing (InterpreterError)
import Loc exposing (Loc)
import Value exposing (Value)


type Outcome a
    = DoneInterpreting (Env Value) a
    | NeedsEffect0 Effect0 (() -> Outcome a)
    | NeedsEffectStr EffectStr (String -> Outcome a)
    | NeedsEffectMaybeStr EffectMaybeStr (Maybe String -> Outcome a)
    | NeedsEffectBool EffectBool (Bool -> Outcome a)
    | FoundError ( Loc, InterpreterError )


succeed : Env Value -> a -> Outcome a
succeed env a =
    DoneInterpreting env a


fail : InterpreterError -> Outcome a
fail err =
    -- TODO use a non-dummy Loc
    FoundError
        ( { row = -1, col = -1 }
        , err
        )


map : (a -> b) -> Outcome a -> Outcome b
map fn outcome =
    case outcome of
        DoneInterpreting env a ->
            DoneInterpreting env (fn a)

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> map fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> map fn)

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff (k >> map fn)

        NeedsEffectBool eff k ->
            NeedsEffectBool eff (k >> map fn)

        FoundError err ->
            FoundError err


mapBoth : (Env Value -> a -> ( Env Value, b )) -> Outcome a -> Outcome b
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

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff (k >> mapBoth fn)

        NeedsEffectBool eff k ->
            NeedsEffectBool eff (k >> mapBoth fn)

        FoundError err ->
            FoundError err


mapEnv : (Env Value -> Env Value) -> Outcome a -> Outcome a
mapEnv fn outcome =
    case outcome of
        DoneInterpreting env a ->
            DoneInterpreting (fn env) a

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> mapEnv fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> mapEnv fn)

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff (k >> mapEnv fn)

        NeedsEffectBool eff k ->
            NeedsEffectBool eff (k >> mapEnv fn)

        FoundError err ->
            FoundError err


attemptMapEnv : (Env Value -> Maybe (Env Value)) -> InterpreterError -> Outcome a -> Outcome a
attemptMapEnv fn error outcome =
    case outcome of
        DoneInterpreting env a ->
            case fn env of
                Nothing ->
                    FoundError ( Debug.todo "interpreter outcome - attempt map env - loc", error )

                Just env_ ->
                    DoneInterpreting env_ a

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> attemptMapEnv fn error)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> attemptMapEnv fn error)

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff (k >> attemptMapEnv fn error)

        NeedsEffectBool eff k ->
            NeedsEffectBool eff (k >> attemptMapEnv fn error)

        FoundError err ->
            FoundError err


andThen : (Env Value -> a -> Outcome b) -> Outcome a -> Outcome b
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

                NeedsEffectMaybeStr eff k ->
                    NeedsEffectMaybeStr eff k

                NeedsEffectBool eff k ->
                    NeedsEffectBool eff k

                FoundError err ->
                    FoundError err

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> andThen fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> andThen fn)

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff (k >> andThen fn)

        NeedsEffectBool eff k ->
            NeedsEffectBool eff (k >> andThen fn)

        FoundError err ->
            FoundError err


onError : (InterpreterError -> Outcome a) -> Outcome a -> Outcome a
onError fn outcome1 =
    case outcome1 of
        FoundError ( _, err ) ->
            fn err

        DoneInterpreting env1 a ->
            DoneInterpreting env1 a

        NeedsEffect0 eff k ->
            NeedsEffect0 eff (k >> onError fn)

        NeedsEffectStr eff k ->
            NeedsEffectStr eff (k >> onError fn)

        NeedsEffectMaybeStr eff k ->
            NeedsEffectMaybeStr eff (k >> onError fn)

        NeedsEffectBool eff k ->
            NeedsEffectBool eff (k >> onError fn)
