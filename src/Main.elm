module Main exposing (Flags, Model, Msg, main)

import AST.Frontend as AST
import Codegen.HVM
import Console
import Debug.Extra
import Desugar
import Effect exposing (Effect0, EffectBool, EffectMaybeStr, EffectStr)
import Env exposing (Env)
import Error exposing (Error(..))
import HVM.AST
import HVM.ToString
import Interpreter
import Interpreter.Outcome as Interpreter
import Lexer
import Loc
import Parser
import Token exposing (Token)
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
    { sourceCode : String
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


logLexed : Result Error (List Token) -> Result Error (List Token)
logLexed result =
    case result of
        Err err ->
            let
                _ =
                    Debug.log
                        (Debug.Extra.standOutErr "err")
                        err
            in
            result

        Ok ts ->
            let
                _ =
                    ts |> List.reverse |> List.map (Token.toDebugString >> Debug.log "lexed")
            in
            result


logParsed : AST.Program -> AST.Program
logParsed ast =
    let
        _ =
            Debug.log (AST.inspect ast) "frontend program"
    in
    ast


isCompilingToHVM : Bool
isCompilingToHVM =
    True


process : File -> Env Value -> ( Model, Cmd Msg )
process { file, content } env =
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
            if isCompilingToHVM then
                case
                    frontendProgram
                        --|> logParsed
                        |> Desugar.desugarProgram
                        |> Result.map Codegen.HVM.codegenProgram
                        |> Result.map HVM.ToString.file
                of
                    Err err ->
                        Debug.todo ("handle desugar error: " ++ Debug.toString err)

                    Ok hvmString ->
                        effect0 (Effect.Println hvmString) <|
                            \() -> initModel

            else
                -- interpreting
                frontendProgram
                    --|> logParsed
                    |> Interpreter.interpretProgram env
                    |> handleInterpreterOutcome


loadStdlib : Bool
loadStdlib =
    False


init : Flags -> ( Model, Cmd Msg )
init flags =
    if loadStdlib then
        initModel
            |> andThenMany (List.map process flags.stdlibSources)
            |> andThen
                (process
                    { file = "main.cara"
                    , content = flags.sourceCode
                    }
                )

    else
        process
            { file = "main.cara"
            , content = flags.sourceCode
            }
            initEnv


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


effect0 : Effect0 -> K () -> ( Model, Cmd Msg )
effect0 eff k =
    ( PausedOnEffect0 eff k
    , Effect.handleEffect0 eff
    )


handleInterpreterOutcome : Interpreter.Outcome () -> ( Model, Cmd Msg )
handleInterpreterOutcome outcome =
    case outcome of
        Interpreter.DoneInterpreting env _ ->
            done env

        Interpreter.FoundError error ->
            exitWithError (InterpreterError error)

        Interpreter.NeedsEffect0 effect k ->
            pauseOnEffect0 effect k

        Interpreter.NeedsEffectStr effect k ->
            pauseOnEffectStr effect k

        Interpreter.NeedsEffectMaybeStr effect k ->
            pauseOnEffectMaybeStr effect k

        Interpreter.NeedsEffectBool effect k ->
            pauseOnEffectBool effect k


exitWithError : Error -> ( Model, Cmd Msg )
exitWithError err =
    ( ExitErr, printError err )


done : Env Value -> ( Model, Cmd Msg )
done env =
    ( PartiallyDone env, Cmd.none )


initEnv : Env Value
initEnv =
    Env.initWithIntrinsics { intrinsicToValue = VIntrinsic }


initModel : ( Model, Cmd Msg )
initModel =
    done initEnv


pauseOnEffect0 : Effect0 -> KO () -> ( Model, Cmd Msg )
pauseOnEffect0 effect k =
    ( PausedOnEffect0 effect (k >> handleInterpreterOutcome)
    , Effect.handleEffect0 effect
    )


pauseOnEffectStr : EffectStr -> KO String -> ( Model, Cmd Msg )
pauseOnEffectStr effect k =
    ( PausedOnEffectStr effect (k >> handleInterpreterOutcome)
    , Effect.handleEffectStr effect
    )


pauseOnEffectMaybeStr : EffectMaybeStr -> KO (Maybe String) -> ( Model, Cmd Msg )
pauseOnEffectMaybeStr effect k =
    ( PausedOnEffectMaybeStr effect (k >> handleInterpreterOutcome)
    , Effect.handleEffectMaybeStr effect
    )


pauseOnEffectBool :
    EffectBool
    -> KO Bool
    -> ( Model, Cmd Msg )
pauseOnEffectBool effect k =
    ( PausedOnEffectBool effect (k >> handleInterpreterOutcome)
    , Effect.handleEffectBool effect
    )


printError : Error -> Cmd msg
printError error =
    --"{LOC} - {TITLE}"
    "{LOC} - {ERROR}"
        |> String.replace "{LOC}" (Loc.toString (Error.loc error))
        --|> String.replace "{TITLE}" (Error.title error)
        |> String.replace "{ERROR}" (Error.inspect error)
        |> Debug.Extra.standOutErr
        |> Effect.eprintln


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
