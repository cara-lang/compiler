module TestRunner exposing (Flags, Model, Msg, main)

import Effect exposing (Effect0, EffectBool, EffectMaybeStr, EffectStr)
import Env
import Error exposing (Details(..), Error)
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


type alias K a =
    a -> ( Model, Cmd Msg )


type alias KO a =
    a -> Interpreter.Outcome ()


type Model
    = Done
    | PausedOnEffect0 Effect0 (K ())
    | PausedOnEffectStr EffectStr (K String)
    | PausedOnEffectMaybeStr EffectMaybeStr (K (Maybe String))
    | PausedOnEffectBool EffectBool (K Bool)


type Msg
    = CompletedChdir
    | CompletedPrint
    | CompletedPrintln
    | CompletedEprintln
    | CompletedReadFile String
    | CompletedReadFileMaybe (Maybe String)
    | CompletedWriteFile
    | CompletedWriteFileMaybe Bool


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


runTest : String -> String -> K () -> ( Model, Cmd Msg )
runTest name fileContents k =
    case
        fileContents
            |> (Lexer.lex >> Result.mapError (\( loc, lexerErr ) -> { loc = loc, details = LexerError lexerErr }))
            |> Result.andThen (Parser.parse >> Result.mapError (\( loc, parserErr ) -> { loc = loc, details = ParserError parserErr }))
    of
        Err err ->
            if String.endsWith "-err" name then
                effectMaybeStr (Effect.ReadFileMaybe { filename = "stderr.txt" }) <| \stderr ->
                let
                    firstLine =
                        stderr
                            |> Maybe.withDefault ""
                            |> String.lines
                            |> List.head
                            |> Maybe.withDefault ""
                in
                if String.startsWith (Error.code err.details) firstLine then
                    k ()

                else
                    effect0 (Effect.Eprintln <| ": " ++ name ++ " | " ++ Error.title err.details) k

            else
                effect0 (Effect.Eprintln <| ": " ++ name ++ " | " ++ Error.title err.details) k

        Ok astTree ->
            effect0 (Effect.Println <| "interpreting: " ++ name) <| \() ->
            astTree
                |> Interpreter.interpretProgram (Env.initWithIntrinsics { intrinsicToValue = VIntrinsic })
                |> handleInterpreterOutcome name k


handleInterpreterOutcome :
    String
    -> K ()
    -> Interpreter.Outcome ()
    -> ( Model, Cmd Msg )
handleInterpreterOutcome testName k outcome =
    case outcome of
        Interpreter.DoneInterpreting _ _ ->
            -- TODO check stdout against stdout.txt
            k ()

        Interpreter.FoundError _ err ->
            -- TODO show the loc somewhere?
            if String.endsWith "-err" testName then
                effectMaybeStr (Effect.ReadFileMaybe { filename = "stderr.txt" }) <| \stderr ->
                let
                    firstLine =
                        stderr
                            |> Maybe.withDefault ""
                            |> String.lines
                            |> List.head
                            |> Maybe.withDefault ""
                in
                if String.startsWith (Error.code (InterpreterError err)) firstLine then
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

        Interpreter.NeedsEffectBool effect kOutcome ->
            pauseOnEffectBool effect kOutcome testName k


pauseOnEffect0 :
    Effect0
    -> KO ()
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
pauseOnEffect0 effect kOutcome testName k =
    ( PausedOnEffect0 effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffect0 effect
    )


pauseOnEffectStr :
    EffectStr
    -> KO String
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
pauseOnEffectStr effect kOutcome testName k =
    ( PausedOnEffectStr effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffectStr effect
    )


pauseOnEffectMaybeStr :
    EffectMaybeStr
    -> KO (Maybe String)
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
pauseOnEffectMaybeStr effect kOutcome testName k =
    ( PausedOnEffectMaybeStr effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffectMaybeStr effect
    )


pauseOnEffectBool :
    EffectBool
    -> KO Bool
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
pauseOnEffectBool effect kOutcome testName k =
    ( PausedOnEffectBool effect (kOutcome >> handleInterpreterOutcome testName k)
    , Effect.handleEffectBool effect
    )


effect0 : Effect0 -> K () -> ( Model, Cmd Msg )
effect0 eff k =
    ( PausedOnEffect0 eff k
    , Effect.handleEffect0 eff
    )


effectStr : EffectStr -> K String -> ( Model, Cmd Msg )
effectStr eff k =
    ( PausedOnEffectStr eff k
    , Effect.handleEffectStr eff
    )


effectMaybeStr : EffectMaybeStr -> K (Maybe String) -> ( Model, Cmd Msg )
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

        PausedOnEffectBool effect k ->
            case ( effect, msg ) of
                ( Effect.WriteFileMaybe _, CompletedWriteFileMaybe result ) ->
                    k result

                ( Effect.WriteFileMaybe _, _ ) ->
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
        , Effect.completedWriteFileMaybe CompletedWriteFileMaybe
        ]
