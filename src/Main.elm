port module Main exposing (main)

import AST exposing (AST(..))
import Effect exposing (Effect)
import Env exposing (Env)
import Error exposing (Error(..), InterpreterError(..))
import Html.Attributes exposing (value)
import Interpreter
import Lexer
import Parser
import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)
import Tree.Zipper.Extra as Zipper
import Value exposing (Value)


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
      --                  ↓ where to continue afterwards
      --                  ↓            ↓ what to do
    | PausedOnEffect Env (Zipper AST) Effect


type Msg
    = CompletedPrintln String
    | CompletedEprintln String
    | CompletedReadFile { filename : String, content : String }
    | CompletedWriteFile { filename : String, content : String }


port println : String -> Cmd msg


port eprintln : String -> Cmd msg


port readFile : { filename : String } -> Cmd msg


port writeFile : { filename : String, content : String } -> Cmd msg


port completedPrintln : (String -> msg) -> Sub msg


port completedEprintln : (String -> msg) -> Sub msg


port completedReadFile : ({ filename : String, content : String } -> msg) -> Sub msg


port completedWriteFile : ({ filename : String, content : String } -> msg) -> Sub msg


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        astResult : Result Error (Tree AST)
        astResult =
            {-
               flags.sourceCode
                   |> (Lexer.lex >> Result.mapError LexerError)
                   |> Result.andThen (Parser.parse >> Result.mapError ParserError)
                   |> Debug.log "AST result"
            -}
            Ok hardcodedProgram
    in
    case astResult of
        Err err ->
            finishWithError err

        Ok astTree ->
            case Interpreter.interpret Env.empty (Zipper.fromTree astTree) of
                Interpreter.DoneInterpreting _ _ _ ->
                    finish

                Interpreter.FoundError _ _ error ->
                    finishWithError (InterpreterError error)

                Interpreter.NeedsEffect env_ pausedZipper effect ->
                    pauseOnEffect env_ pausedZipper effect


finishWithError : Error -> ( Model, Cmd Msg )
finishWithError err =
    ( Done, printError err )


finish : ( Model, Cmd Msg )
finish =
    ( Done, Cmd.none )


pauseOnEffect : Env -> Zipper AST -> Effect -> ( Model, Cmd Msg )
pauseOnEffect env ast effect =
    ( PausedOnEffect env ast effect
    , handleEffect effect
    )


printError : Error -> Cmd msg
printError error =
    eprintln (Error.toString error)


printValue : Value -> Cmd msg
printValue value =
    println (Value.toString value)


handleEffect : Effect -> Cmd Msg
handleEffect effect =
    case effect of
        Effect.Println string ->
            println string

        Effect.Eprintln string ->
            eprintln string

        Effect.ReadFile r ->
            readFile r

        Effect.WriteFile r ->
            writeFile r


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Done ->
            Debug.todo <| "it's weird we're getting a Msg when we're done: " ++ Debug.toString msg

        PausedOnEffect env pausedZipper effect ->
            case ( effect, msg ) of
                ( Effect.Println _, CompletedPrintln _ ) ->
                    continueAfterEffect env pausedZipper

                ( Effect.Eprintln _, CompletedEprintln _ ) ->
                    continueAfterEffect env pausedZipper

                ( Effect.ReadFile _, CompletedReadFile { content } ) ->
                    continueAfterEffectWithString content env pausedZipper

                ( Effect.WriteFile _, CompletedWriteFile _ ) ->
                    continueAfterEffect env pausedZipper

                _ ->
                    Debug.todo <| "msg effect mismatch: " ++ Debug.toString ( effect, msg )


continueAfterEffect : Env -> Zipper AST -> ( Model, Cmd Msg )
continueAfterEffect env ast =
    -- Since we're removing things as they get finished, we can start everything from beginning
    case Interpreter.interpret (Zipper.root env) (Zipper.root ast) of
        Interpreter.DoneInterpreting _ _ _ ->
            finish

        Interpreter.FoundError _ _ error ->
            finishWithError (InterpreterError error)

        Interpreter.NeedsEffect env_ pausedZipper effect ->
            pauseOnEffect env_ pausedZipper effect


continueAfterEffectWithString : String -> Env -> Zipper AST -> ( Model, Cmd Msg )
continueAfterEffectWithString string env doneZipper =
    Debug.todo "continueAfterEffectWithString"


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ completedPrintln CompletedPrintln
        , completedEprintln CompletedEprintln
        , completedReadFile CompletedReadFile
        , completedWriteFile CompletedWriteFile
        ]


hardcodedProgram : Tree AST
hardcodedProgram =
    let
        t =
            Tree.tree

        s =
            Tree.singleton

        letX n =
            t (Let { name = "x" }) [ s (Int n) ]

        prnX =
            t Println [ s (Var { qualifiers = [], name = "x" }) ]

        module_ name children =
            t (Module { name = name }) children
    in
    t (Program { filename = "main.cara" })
        [ letX 1
        , module_ "Foo"
            [ prnX -- 1
            , letX 2
            , prnX -- 2
            , module_ "Bar"
                [ prnX -- 2
                , letX 3
                , prnX -- 3
                , t Println [ s (Var { qualifiers = [ "Foo" ], name = "x" }) ] -- 2
                , t Println [ s (RootVar { qualifiers = [], name = "x" }) ] -- 1
                , t Println [ s (RootVar { qualifiers = [ "Foo" ], name = "x" }) ] -- 2
                , t Println [ s (RootVar { qualifiers = [ "Foo", "Bar" ], name = "x" }) ] -- 3
                ]
            , t Println [ s (Var { qualifiers = [], name = "x" }) ] -- 2
            , t Println [ s (Var { qualifiers = [ "Foo" ], name = "x" }) ] -- 2
            , t Println [ s (Var { qualifiers = [ "Bar" ], name = "x" }) ] -- 3
            , t Println [ s (Var { qualifiers = [ "Foo", "Bar" ], name = "x" }) ] -- 3
            , t Println [ s (RootVar { qualifiers = [], name = "x" }) ] -- 1
            , t Println [ s (RootVar { qualifiers = [ "Foo" ], name = "x" }) ] -- 2
            , t Println [ s (RootVar { qualifiers = [ "Foo", "Bar" ], name = "x" }) ] -- 3
            ]
        , t Println [ s (Var { qualifiers = [], name = "x" }) ] -- 1
        , t Println [ s (Var { qualifiers = [ "Foo" ], name = "x" }) ] -- 2
        , t Println [ s (Var { qualifiers = [ "Foo", "Bar" ], name = "x" }) ] -- 3
        , t Println [ s (RootVar { qualifiers = [], name = "x" }) ] -- 1
        , t Println [ s (RootVar { qualifiers = [ "Foo" ], name = "x" }) ] -- 2
        , t Println [ s (RootVar { qualifiers = [ "Foo", "Bar" ], name = "x" }) ] -- 3
        ]
