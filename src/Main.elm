module Main exposing (Flags, Model, Msg, main)

import AST
import Effect exposing (Effect0, EffectStr)
import Env
import Error exposing (Error(..))
import Interpreter
import Interpreter.Outcome as Interpreter
import Lexer
import Parser


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { sourceCode : String
    }


type Model
    = Done
    | ExitingWithError
    | PausedOnEffect0 Effect0 (() -> ( Model, Cmd Msg ))
    | PausedOnEffectStr EffectStr (String -> ( Model, Cmd Msg ))


type Msg
    = CompletedChdir
    | CompletedPrint
    | CompletedPrintln
    | CompletedEprintln
    | CompletedReadFile String
    | CompletedWriteFile


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        astResult : Result Error AST.Program
        astResult =
            flags.sourceCode
                |> (Lexer.lex >> Result.mapError LexerError)
                --|> Result.map (\ts -> let _ = ts |> List.map .type_ |> Debug.log "lexed" in ts)
                |> Result.andThen (Parser.parse >> Result.mapError ParserError)
    in
    case astResult of
        Err err ->
            finishWithError err

        Ok astTree ->
            astTree
                |> Interpreter.interpretProgram Env.initWithIntrinsics
                |> handleInterpreterOutcome


handleInterpreterOutcome : Interpreter.Outcome () -> ( Model, Cmd Msg )
handleInterpreterOutcome outcome =
    case outcome of
        Interpreter.DoneInterpreting _ _ ->
            finish

        Interpreter.FoundError error ->
            finishWithError (InterpreterError error)

        Interpreter.NeedsEffect0 effect k ->
            pauseOnEffect0 effect k

        Interpreter.NeedsEffectStr effect k ->
            pauseOnEffectStr effect k


finishWithError : Error -> ( Model, Cmd Msg )
finishWithError err =
    -- TODO show the location of the error (in Lexer, in Parser, in Interpreter)
    ( ExitingWithError, printError err )


finish : ( Model, Cmd Msg )
finish =
    ( Done, Cmd.none )


pauseOnEffect0 : Effect0 -> (() -> Interpreter.Outcome ()) -> ( Model, Cmd Msg )
pauseOnEffect0 effect k =
    ( PausedOnEffect0 effect (k >> handleInterpreterOutcome)
    , Effect.handleEffect0 effect
    )


pauseOnEffectStr : EffectStr -> (String -> Interpreter.Outcome ()) -> ( Model, Cmd Msg )
pauseOnEffectStr effect k =
    ( PausedOnEffectStr effect (k >> handleInterpreterOutcome)
    , Effect.handleEffectStr effect
    )


printError : Error -> Cmd msg
printError error =
    Effect.eprintln (Error.title error)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Done ->
            Debug.todo <| "BUG: we're getting a Msg when we're Done: " ++ Debug.toString msg

        ExitingWithError ->
            case msg of
                CompletedEprintln ->
                    ( Done, Cmd.none )

                _ ->
                    Debug.todo <| "BUG: we're getting a non-eprintln Msg when ExitingWithError: " ++ Debug.toString msg

        PausedOnEffect0 effect k ->
            case ( effect, msg ) of
                ( Effect.Print _, CompletedPrint ) ->
                    k ()

                ( Effect.Print _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )

                ( Effect.Println _, CompletedPrintln ) ->
                    k ()

                ( Effect.Println _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )

                ( Effect.Eprintln _, CompletedEprintln ) ->
                    k ()

                ( Effect.Eprintln _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )

                ( Effect.WriteFile _, CompletedWriteFile ) ->
                    k ()

                ( Effect.WriteFile _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )

                ( Effect.Chdir _, CompletedChdir ) ->
                    k ()

                ( Effect.Chdir _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )

        PausedOnEffectStr effect k ->
            case ( effect, msg ) of
                ( Effect.ReadFile _, CompletedReadFile content ) ->
                    k content

                ( Effect.ReadFile _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Effect.completedChdir (\_ -> CompletedChdir)
        , Effect.completedPrint (\_ -> CompletedPrint)
        , Effect.completedPrintln (\_ -> CompletedPrintln)
        , Effect.completedEprintln (\_ -> CompletedEprintln)
        , Effect.completedReadFile CompletedReadFile
        , Effect.completedWriteFile (\_ -> CompletedWriteFile)
        ]
