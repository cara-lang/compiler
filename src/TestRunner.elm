module TestRunner exposing (Flags, Model, Msg, main)

{-| TODO: start testing the HVM compilation as well! Right now we're only doing the interpreter part.
-}

import AST.Frontend as AST
import Console
import Debug.Extra
import Effect exposing (Effect0, EffectBool, EffectMaybeStr, EffectStr)
import Env exposing (Env)
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


type alias File =
    { file : String
    , content : String
    }


type alias Flags =
    { dirs : List String
    , rootPath : String
    , stdlibSources : List File
    }


type alias K a =
    a -> ( Model, Cmd Msg )


type alias KO a =
    a -> Interpreter.Outcome ()


type Model
    = PartiallyDone (Env Value)
    | ExitOk
    | ExitErr
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


initEnv : Env Value
initEnv =
    Env.initWithIntrinsics { intrinsicToValue = VIntrinsic }


initModel : ( Model, Cmd Msg )
initModel =
    done initEnv


processStdlib : File -> Env Value -> ( Model, Cmd Msg )
processStdlib { file, content } env =
    let
        _ =
            Debug.log (Console.blue "processing") file
    in
    let
        astResult : Result Error AST.Program
        astResult =
            content
                |> (Lexer.lex >> Result.mapError LexerError)
                --|> logLexed
                |> Result.andThen (Parser.parse >> Result.mapError ParserError)
    in
    case astResult of
        Err err ->
            exitWithError err

        Ok frontendProgram ->
            -- interpreting
            frontendProgram
                --|> logParsed
                |> Interpreter.interpretProgram env
                |> handleStdlibInterpreterOutcome


init : Flags -> ( Model, Cmd Msg )
init flags =
    initModel
        |> andThenMany (List.map processStdlib flags.stdlibSources)
        |> andThen (runTests flags.rootPath flags.dirs)


exitWithError : Error -> ( Model, Cmd Msg )
exitWithError err =
    ( ExitErr, printError err )


printError : Error -> Cmd msg
printError error =
    --"{LOC} - {TITLE}"
    "{LOC} - {ERROR}"
        |> String.replace "{LOC}" (Loc.toString (Error.loc error))
        --|> String.replace "{TITLE}" (Error.title error)
        |> String.replace "{ERROR}" (Error.inspect error)
        |> Debug.Extra.standOutErr
        |> Effect.eprintln


done : Env Value -> ( Model, Cmd Msg )
done env =
    ( PartiallyDone env, Cmd.none )


andThenMany : List (K (Env Value)) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenMany ks ( model, cmd ) =
    case ( ks, model ) of
        ( [], _ ) ->
            ( model, cmd )

        ( k :: rest, ExitOk ) ->
            ( model, cmd )

        ( k :: rest, _ ) ->
            andThenMany rest (andThen k ( model, cmd ))


andThen : K (Env Value) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen k ( model, cmd ) =
    case model of
        PartiallyDone env ->
            let
                ( newModel, newCmd ) =
                    k env
            in
            ( newModel
            , Cmd.batch [ cmd, newCmd ]
            )

        ExitOk ->
            ( model, cmd )

        ExitErr ->
            ( model, cmd )

        PausedOnEffect0 eff k2 ->
            Debug.todo "andThen p0"

        PausedOnEffectStr eff k2 ->
            Debug.todo "andThen pStr"

        PausedOnEffectMaybeStr eff k2 ->
            Debug.todo "andThen pMaybeStr"

        PausedOnEffectBool eff k2 ->
            Debug.todo "andThen pBool"


runTests : String -> List String -> Env Value -> ( Model, Cmd Msg )
runTests rootPath testDirs stdlibEnv =
    case testDirs of
        [] ->
            done stdlibEnv

        testDir :: rest ->
            effect0 (Effect.Chdir testDir) <| \() ->
            effectStr (Effect.ReadFile { filename = "main.cara" }) <| \fileContents ->
            runTest testDir fileContents <| \() ->
            effect0 (Effect.Chdir rootPath) <| \() ->
            -- separate test runs don't affect each others' Envs:
            runTests rootPath rest stdlibEnv


runTest : String -> String -> K () -> ( Model, Cmd Msg )
runTest name fileContents k =
    case
        fileContents
            |> (Lexer.lex >> Result.mapError LexerError)
            |> Result.andThen (Parser.parse >> Result.mapError ParserError)
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
                if String.startsWith (Error.code err) firstLine then
                    k ()

                else
                    effect0 (Effect.Eprintln <| ": " ++ name ++ " | " ++ Error.title err) k

            else
                effect0 (Effect.Eprintln <| ": " ++ name ++ " | " ++ Error.title err) k

        Ok astTree ->
            effect0 (Effect.Println <| Console.blue "interpreting: " ++ name) <| \() ->
            astTree
                |> Interpreter.interpretProgram (Env.initWithIntrinsics { intrinsicToValue = VIntrinsic })
                |> handleTestInterpreterOutcome name k


handleStdlibInterpreterOutcome : Interpreter.Outcome () -> ( Model, Cmd Msg )
handleStdlibInterpreterOutcome outcome =
    case outcome of
        Interpreter.DoneInterpreting env _ ->
            done env

        Interpreter.FoundError error ->
            exitWithError (InterpreterError error)

        Interpreter.NeedsEffect0 effect k ->
            stdlibPauseOnEffect0 effect k

        Interpreter.NeedsEffectStr effect k ->
            stdlibPauseOnEffectStr effect k

        Interpreter.NeedsEffectMaybeStr effect k ->
            stdlibPauseOnEffectMaybeStr effect k

        Interpreter.NeedsEffectBool effect k ->
            stdlibPauseOnEffectBool effect k


stdlibPauseOnEffect0 : Effect0 -> KO () -> ( Model, Cmd Msg )
stdlibPauseOnEffect0 effect k =
    ( PausedOnEffect0 effect (k >> handleStdlibInterpreterOutcome)
    , Effect.handleEffect0 effect
    )


stdlibPauseOnEffectStr : EffectStr -> KO String -> ( Model, Cmd Msg )
stdlibPauseOnEffectStr effect k =
    ( PausedOnEffectStr effect (k >> handleStdlibInterpreterOutcome)
    , Effect.handleEffectStr effect
    )


stdlibPauseOnEffectMaybeStr : EffectMaybeStr -> KO (Maybe String) -> ( Model, Cmd Msg )
stdlibPauseOnEffectMaybeStr effect k =
    ( PausedOnEffectMaybeStr effect (k >> handleStdlibInterpreterOutcome)
    , Effect.handleEffectMaybeStr effect
    )


stdlibPauseOnEffectBool :
    EffectBool
    -> KO Bool
    -> ( Model, Cmd Msg )
stdlibPauseOnEffectBool effect k =
    ( PausedOnEffectBool effect (k >> handleStdlibInterpreterOutcome)
    , Effect.handleEffectBool effect
    )


handleTestInterpreterOutcome :
    String
    -> K ()
    -> Interpreter.Outcome ()
    -> ( Model, Cmd Msg )
handleTestInterpreterOutcome testName k outcome =
    case outcome of
        Interpreter.DoneInterpreting _ _ ->
            -- TODO check stdout against stdout.txt
            k ()

        Interpreter.FoundError err ->
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
            testPauseOnEffect0 effect kOutcome testName k

        Interpreter.NeedsEffectStr effect kOutcome ->
            testPauseOnEffectStr effect kOutcome testName k

        Interpreter.NeedsEffectMaybeStr effect kOutcome ->
            testPauseOnEffectMaybeStr effect kOutcome testName k

        Interpreter.NeedsEffectBool effect kOutcome ->
            testPauseOnEffectBool effect kOutcome testName k


testPauseOnEffect0 :
    Effect0
    -> KO ()
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
testPauseOnEffect0 effect kOutcome testName k =
    ( PausedOnEffect0 effect (kOutcome >> handleTestInterpreterOutcome testName k)
    , Effect.handleEffect0 effect
    )


testPauseOnEffectStr :
    EffectStr
    -> KO String
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
testPauseOnEffectStr effect kOutcome testName k =
    ( PausedOnEffectStr effect (kOutcome >> handleTestInterpreterOutcome testName k)
    , Effect.handleEffectStr effect
    )


testPauseOnEffectMaybeStr :
    EffectMaybeStr
    -> KO (Maybe String)
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
testPauseOnEffectMaybeStr effect kOutcome testName k =
    ( PausedOnEffectMaybeStr effect (kOutcome >> handleTestInterpreterOutcome testName k)
    , Effect.handleEffectMaybeStr effect
    )


testPauseOnEffectBool :
    EffectBool
    -> KO Bool
    -> String
    -> K ()
    -> ( Model, Cmd Msg )
testPauseOnEffectBool effect kOutcome testName k =
    ( PausedOnEffectBool effect (kOutcome >> handleTestInterpreterOutcome testName k)
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
    let
        mismatch effect =
            Debug.Extra.todo1 "Effect mismatch" ( effect, msg )
    in
    case model of
        PartiallyDone env ->
            Debug.Extra.todo1 "update PartiallyDone" msg

        ExitOk ->
            Debug.Extra.todo1 "BUG: we're getting a Msg when we're ExitOk'ed" msg

        ExitErr ->
            case msg of
                CompletedEprintln ->
                    ( ExitOk, Cmd.none )

                _ ->
                    Debug.Extra.todo1 "BUG: we're getting a non-eprintln Msg when ExitErr" msg

        PausedOnEffect0 effect k ->
            case ( effect, msg ) of
                ( Effect.Print _, CompletedPrint ) ->
                    k ()

                ( Effect.Print _, _ ) ->
                    mismatch effect

                ( Effect.Println _, CompletedPrintln ) ->
                    k ()

                ( Effect.Println _, _ ) ->
                    mismatch effect

                ( Effect.Eprintln _, CompletedEprintln ) ->
                    k ()

                ( Effect.Eprintln _, _ ) ->
                    mismatch effect

                ( Effect.WriteFile _, CompletedWriteFile ) ->
                    k ()

                ( Effect.WriteFile _, _ ) ->
                    mismatch effect

                ( Effect.Chdir _, CompletedChdir ) ->
                    k ()

                ( Effect.Chdir _, _ ) ->
                    mismatch effect

        PausedOnEffectStr effect k ->
            case ( effect, msg ) of
                ( Effect.ReadFile _, CompletedReadFile content ) ->
                    k content

                ( Effect.ReadFile _, _ ) ->
                    mismatch effect

        PausedOnEffectMaybeStr effect k ->
            case ( effect, msg ) of
                ( Effect.ReadFileMaybe _, CompletedReadFileMaybe content ) ->
                    k content

                ( Effect.ReadFileMaybe _, _ ) ->
                    mismatch effect

        PausedOnEffectBool effect k ->
            case ( effect, msg ) of
                ( Effect.WriteFileMaybe _, CompletedWriteFileMaybe result ) ->
                    k result

                ( Effect.WriteFileMaybe _, _ ) ->
                    mismatch effect


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
