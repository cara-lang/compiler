module Main exposing (Flags, Model, Msg, main)

import AST.Frontend as AST
import Codegen.HVM
import Desugar
import Effect exposing (Effect0, EffectBool, EffectMaybeStr, EffectStr)
import Env
import Error exposing (Details(..), Error)
import HVM.AST
import HVM.ToString
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
    { sourceCode : String
    }


type alias K a =
    a -> ( Model, Cmd Msg )


type alias KO a =
    a -> Interpreter.Outcome ()


type Model
    = Done
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


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        astResult : Result Error AST.Program
        astResult =
            flags.sourceCode
                |> (Lexer.lex >> Result.mapError (\( loc, lexerErr ) -> { loc = loc, details = LexerError lexerErr }))
                --|> Result.map (\ts -> let _ = ts |> List.reverse |> List.map (.type_ >> Debug.log "lexed") in ts)
                |> Result.andThen (Parser.parse >> Result.mapError (\( loc, parserErr ) -> { loc = loc, details = ParserError parserErr }))
    in
    case astResult of
        Err err ->
            finishWithError err

        Ok frontendProgram ->
            {-
               frontendProgram
                   |> Interpreter.interpretProgram (Env.initWithIntrinsics { intrinsicToValue = VIntrinsic })
                   |> handleInterpreterOutcome
            -}
            case
                frontendProgram
                    |> Desugar.desugarProgram
                    |> Result.map Codegen.HVM.codegenProgram
                    |> Result.map HVM.ToString.file
            of
                Err _ ->
                    Debug.todo "handle desugar error"

                Ok hvmString ->
                    effect0 (Effect.WriteFile { filename = "example.hvm", content = hvmString }) <|
                        \() ->
                            finish


effect0 : Effect0 -> K () -> ( Model, Cmd Msg )
effect0 eff k =
    ( PausedOnEffect0 eff k
    , Effect.handleEffect0 eff
    )


handleInterpreterOutcome : Interpreter.Outcome () -> ( Model, Cmd Msg )
handleInterpreterOutcome outcome =
    case outcome of
        Interpreter.DoneInterpreting _ _ ->
            finish

        Interpreter.FoundError loc error ->
            finishWithError
                { loc = loc
                , details = InterpreterError error
                }

        Interpreter.NeedsEffect0 effect k ->
            pauseOnEffect0 effect k

        Interpreter.NeedsEffectStr effect k ->
            pauseOnEffectStr effect k

        Interpreter.NeedsEffectMaybeStr effect k ->
            pauseOnEffectMaybeStr effect k

        Interpreter.NeedsEffectBool effect k ->
            pauseOnEffectBool effect k


finishWithError : Error -> ( Model, Cmd Msg )
finishWithError err =
    ( ExitingWithError, printError err )


finish : ( Model, Cmd Msg )
finish =
    ( Done, Cmd.none )


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
    "{LOC} - {TITLE}"
        |> String.replace "{LOC}" (Loc.toString error.loc)
        |> String.replace "{TITLE}" (Error.title error.details)
        |> Effect.eprintln


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
