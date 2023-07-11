module TestRunner exposing (Flags, Model, Msg, main)

import Effect exposing (Effect0, EffectMaybeStr, EffectStr)
import Env
import Error exposing (Error(..))
import Interpreter
import Interpreter.Outcome as Interpreter
import Lexer
import Loc
import Parser
import Value exposing (Value(..))


main : Program Flags Model Msg
main =
    Platform.worker
        { init = init
        , update = update
        , subscriptions = subscriptions
        }


type alias Flags =
    { dirs : List String
    , rootPath : String
    }


type Model
    = Done
    | PausedOnEffect0 Effect0 (() -> ( Model, Cmd Msg ))
    | PausedOnEffectStr EffectStr (String -> ( Model, Cmd Msg ))
    | PausedOnEffectMaybeStr EffectMaybeStr (Maybe String -> ( Model, Cmd Msg ))


type Msg
    = CompletedChdir
    | CompletedPrint
    | CompletedPrintln
    | CompletedEprintln
    | CompletedReadFile String
    | CompletedReadFileMaybe (Maybe String)
    | CompletedWriteFile


init : Flags -> ( Model, Cmd Msg )
init flags =
    effect0 (Effect.Println "Running tests") <| \() ->
    effect0 (Effect.Println "----------------------------") <| \() ->
    runTests flags.rootPath flags.dirs


runTests : String -> List String -> ( Model, Cmd Msg )
runTests rootPath testDirs =
    case testDirs of
        [] ->
            effect0 (Effect.Println "----------------------------") <| \() ->
            effect0 (Effect.Println "Done running tests!") <| \() ->
            ( Done, Cmd.none )

        testDir :: rest ->
            effect0 (Effect.Chdir testDir) <| \() ->
            effectStr (Effect.ReadFile { filename = "main.cara" }) <| \fileContents ->
            runTest testDir fileContents <| \() ->
            effect0 (Effect.Chdir rootPath) <| \() ->
            runTests rootPath rest


runTest : String -> String -> (() -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
runTest name fileContents k =
    case
        fileContents
            |> (Lexer.lex >> Result.mapError LexerError)
            |> Result.andThen (Parser.parse >> Result.mapError ParserError)
    of
        Err err ->
            if String.endsWith "-err" name then
                effectMaybeStr (Effect.ReadFileMaybe { filename = "stderr.txt" }) <| \stderr ->
                if Maybe.andThen (List.head << String.lines) stderr == Just (Error.title err) then
                    k ()

                else
                    effect0 (Effect.Eprintln <| ": " ++ name ++ " | " ++ Error.title err) k

            else
                effect0 (Effect.Eprintln <| ": " ++ name ++ " | " ++ Error.title err) k

        Ok astTree ->
            effect0 (Effect.Println <| "interpreting: " ++ name) <| \() ->
            astTree
                |> Interpreter.interpretProgram (Env.initWithIntrinsics { intrinsicToValue = VIntrinsic })
                |> handleInterpreterOutcome name k


handleInterpreterOutcome :
    String
    -> (() -> ( Model, Cmd Msg ))
    -> Interpreter.Outcome ()
    -> ( Model, Cmd Msg )
handleInterpreterOutcome testName k outcome =
    case outcome of
        Interpreter.DoneInterpreting _ _ ->
            -- TODO check stdout
            k ()

        Interpreter.FoundError err ->
            if String.endsWith "-err" testName then
                effectMaybeStr (Effect.ReadFileMaybe { filename = "stderr.txt" }) <| \stderr ->
                if Maybe.andThen (List.head << String.lines) stderr == Just (Error.title (InterpreterError err)) then
                    k ()

                else
                    effect0 (Effect.Eprintln <| ": " ++ testName ++ " | " ++ Error.title (InterpreterError err)) k

            else
                effect0 (Effect.Eprintln <| ": " ++ testName ++ " | " ++ Error.title (InterpreterError err)) k

        Interpreter.NeedsEffect0 effect kOutcome ->
            pauseOnEffect0 effect kOutcome testName k

        Interpreter.NeedsEffectStr effect kOutcome ->
            pauseOnEffectStr effect kOutcome testName k

        Interpreter.NeedsEffectMaybeStr effect kOutcome ->
            pauseOnEffectMaybeStr effect kOutcome testName k


pauseOnEffect0 :
    Effect0
    -> (() -> Interpreter.Outcome ())
    -> String
    -> (() -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
pauseOnEffect0 effect kOutcome testName k =
    ( PausedOnEffect0 effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffect0 effect
    )


pauseOnEffectStr :
    EffectStr
    -> (String -> Interpreter.Outcome ())
    -> String
    -> (() -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
pauseOnEffectStr effect kOutcome testName k =
    ( PausedOnEffectStr effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffectStr effect
    )


pauseOnEffectMaybeStr :
    EffectMaybeStr
    -> (Maybe String -> Interpreter.Outcome ())
    -> String
    -> (() -> ( Model, Cmd Msg ))
    -> ( Model, Cmd Msg )
pauseOnEffectMaybeStr effect kOutcome testName k =
    ( PausedOnEffectMaybeStr effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffectMaybeStr effect
    )


effect0 : Effect0 -> (() -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
effect0 eff k =
    ( PausedOnEffect0 eff k
    , Effect.handleEffect0 eff
    )


effectStr : EffectStr -> (String -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
effectStr eff k =
    ( PausedOnEffectStr eff k
    , Effect.handleEffectStr eff
    )


effectMaybeStr : EffectMaybeStr -> (Maybe String -> ( Model, Cmd Msg )) -> ( Model, Cmd Msg )
effectMaybeStr eff k =
    ( PausedOnEffectMaybeStr eff k
    , Effect.handleEffectMaybeStr eff
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Done ->
            Debug.todo <| "BUG: we're getting a Msg when we're Done: " ++ Debug.toString msg

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

        PausedOnEffectMaybeStr effect k ->
            case ( effect, msg ) of
                ( Effect.ReadFileMaybe _, CompletedReadFileMaybe content ) ->
                    k content

                ( Effect.ReadFileMaybe _, _ ) ->
                    Debug.todo <| "Effect mismatch: " ++ Debug.toString ( effect, msg )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Effect.completedChdir (\_ -> CompletedChdir)
        , Effect.completedPrint (\_ -> CompletedPrint)
        , Effect.completedPrintln (\_ -> CompletedPrintln)
        , Effect.completedEprintln (\_ -> CompletedEprintln)
        , Effect.completedReadFile CompletedReadFile
        , Effect.completedReadFileMaybe CompletedReadFileMaybe
        , Effect.completedWriteFile (\_ -> CompletedWriteFile)
        ]
