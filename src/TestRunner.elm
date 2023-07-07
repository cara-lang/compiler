module TestRunner exposing (Flags, Model, Msg, main)

import Effect exposing (Effect0, EffectStr)
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


type Msg
    = CompletedChdir
    | CompletedPrint
    | CompletedPrintln
    | CompletedEprintln
    | CompletedReadFile String
    | CompletedWriteFile


init : Flags -> ( Model, Cmd Msg )
init flags =
    effect0 (Effect.Println "----------------------------") <| \() ->
    effect0 (Effect.Println "Running tests") <| \() ->
    effect0 (Effect.Println "----------------------------") <| \() ->
    runTests flags.rootPath flags.dirs


runTests : String -> List String -> ( Model, Cmd Msg )
runTests rootPath testDirs =
    case testDirs of
        [] ->
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
        Err (LexerError ( loc, err )) ->
            if String.endsWith "-err" name then
                -- TODO and if the err is actually the one we want!
                k ()

            else
                effect0 (Effect.Println <| name ++ " | Lexer | " ++ Loc.toString loc ++ " | " ++ Debug.toString err) k

        Err (ParserError ( loc, err )) ->
            if String.endsWith "-err" name then
                -- TODO and if the err is actually the one we want!
                k ()

            else
                effect0 (Effect.Println <| name ++ " | Parser | " ++ Loc.toString loc ++ " | " ++ Debug.toString err) k

        Err (InterpreterError err) ->
            if String.endsWith "-err" name then
                -- TODO and if the err is actually the one we want!
                k ()

            else
                effect0 (Effect.Println <| name ++ " | Interpreter | " ++ Debug.toString err) k

        Ok astTree ->
            astTree
                |> Interpreter.interpretProgram (Env.initWithIntrinsics { intrinsicToValue = VIntrinsic })
                |> handleInterpreterOutcome


handleInterpreterOutcome : Interpreter.Outcome () -> ( Model, Cmd Msg )
handleInterpreterOutcome outcome =
    case outcome of
        Interpreter.DoneInterpreting _ _ ->
            finish

        Interpreter.FoundError error ->
            Debug.todo "mark failure, continue with other tests"

        Interpreter.NeedsEffect0 effect k ->
            pauseOnEffect0 effect k

        Interpreter.NeedsEffectStr effect k ->
            pauseOnEffectStr effect k


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
