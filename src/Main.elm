module Main exposing (Flags, Model, Msg, main)

import AST.Frontend as AST
import Codegen.HVM
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
    = Exit
    | Done (Env Value)
    | ExitingWithError
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
                    Debug.log "err" err
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
    False


process : File -> Env Value -> ( Model, Cmd Msg )
process { file, content } env =
    let
        _ =
            Debug.log "processing" file
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    initModel
        |> andThenMany (List.map process flags.stdlibSources)
        |> andThen
            (process
                { file = "main.cara"
                , content = flags.sourceCode
                }
            )


andThenMany : List (K (Env Value)) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThenMany ks ( model, cmd ) =
    case ( ks, model ) of
        ( [], _ ) ->
            ( model, cmd )

        ( k :: rest, Exit ) ->
            ( model, cmd )

        ( k :: rest, _ ) ->
            andThenMany rest (andThen k ( model, cmd ))


andThen : K (Env Value) -> ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
andThen k ( model, cmd ) =
    case model of
        Exit ->
            ( model, cmd )

        Done env ->
            let
                ( newModel, newCmd ) =
                    k env
            in
            ( newModel
            , Cmd.batch [ cmd, newCmd ]
            )

        ExitingWithError ->
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
    ( ExitingWithError, printError err )


done : Env Value -> ( Model, Cmd Msg )
done env =
    ( Done env, Cmd.none )


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
        |> Effect.eprintln


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Exit ->
            Debug.Extra.todo1 "BUG: we're getting a Msg when we're Exit'ed" msg

        Done env ->
            Debug.Extra.todo1 "update Done" msg

        ExitingWithError ->
            case msg of
                CompletedEprintln ->
                    ( Exit, Cmd.none )

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
