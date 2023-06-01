port module Main exposing (Flags, Model, Msg, main)

import AST
    exposing
        ( Bang(..)
        , Decl(..)
        , Expr(..)
        , LetModifier(..)
        , ModuleModifier(..)
        , Pattern(..)
        , Stmt(..)
        )
import Effect exposing (Effect0, EffectStr)
import Env exposing (Env)
import Error exposing (Error(..))
import Interpreter
import Interpreter.Outcome as Interpreter
import Lexer
import Parser
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


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
    = CompletedPrintln
    | CompletedEprintln
    | CompletedReadFile String
    | CompletedWriteFile



-- EFFECTS


port println : String -> Cmd msg


port eprintln : String -> Cmd msg


port readFile : { filename : String } -> Cmd msg


port writeFile : { filename : String, content : String } -> Cmd msg


port completedPrintln : (() -> msg) -> Sub msg


port completedEprintln : (() -> msg) -> Sub msg


port completedReadFile : (String -> msg) -> Sub msg


port completedWriteFile : (() -> msg) -> Sub msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        astResult : Result Error AST.Program
        astResult =
            flags.sourceCode
                |> (Lexer.lex >> Result.mapError LexerError)
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
    , handleEffect0 effect
    )


pauseOnEffectStr : EffectStr -> (String -> Interpreter.Outcome ()) -> ( Model, Cmd Msg )
pauseOnEffectStr effect k =
    ( PausedOnEffectStr effect (k >> handleInterpreterOutcome)
    , handleEffectStr effect
    )


printError : Error -> Cmd msg
printError error =
    eprintln (Error.title error)


handleEffect0 : Effect0 -> Cmd Msg
handleEffect0 effect =
    case effect of
        Effect.Println string ->
            println string

        Effect.Eprintln string ->
            eprintln string

        Effect.WriteFile r ->
            writeFile r


handleEffectStr : EffectStr -> Cmd Msg
handleEffectStr effect =
    case effect of
        Effect.ReadFile r ->
            readFile r


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
                ( Effect.Println _, CompletedPrintln ) ->
                    k ()

                ( Effect.Eprintln _, CompletedEprintln ) ->
                    k ()

                ( Effect.WriteFile _, CompletedWriteFile ) ->
                    k ()

                _ ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )

        PausedOnEffectStr effect k ->
            case ( effect, msg ) of
                ( Effect.ReadFile _, CompletedReadFile content ) ->
                    k content

                _ ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ completedPrintln (\_ -> CompletedPrintln)
        , completedEprintln (\_ -> CompletedEprintln)
        , completedReadFile CompletedReadFile
        , completedWriteFile (\_ -> CompletedWriteFile)
        ]
