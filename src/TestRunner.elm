module TestRunner exposing (Flags, Model, Msg, main)

import Effect exposing (Effect0, EffectStr)
import Error exposing (Error(..))
import Lexer
import Loc
import Parser


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
    | ExitingWithError
    | PausedOnEffect0 Effect0 (() -> ( Model, Cmd Msg ))
    | PausedOnEffectStr EffectStr (String -> ( Model, Cmd Msg ))
    | RunningTests { rootPath : String, testDirs : List String }


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
    effect0 (Effect.Println "Running lexer + parser tests") <| \() ->
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
    effect0 (Effect.Print <| name ++ ": ") <| \() ->
    let
        parseResult =
            fileContents
                |> (Lexer.lex >> Result.mapError LexerError)
                |> Result.andThen (Parser.parse >> Result.mapError ParserError)
    in
    case parseResult of
        Err (LexerError ( loc, err )) ->
            effect0 (Effect.Println <| "Lexer - " ++ Loc.toString loc ++ " - " ++ Debug.toString err) k

        Err (ParserError ( loc, err )) ->
            effect0 (Effect.Println <| "Parser - " ++ Loc.toString loc ++ " - " ++ Debug.toString err) k

        other ->
            effect0 (Effect.Println <| Debug.toString other) k


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

        RunningTests testDirs ->
            Debug.todo <| "update - running tests - msg: " ++ Debug.toString msg


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
