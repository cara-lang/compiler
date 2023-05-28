module Error exposing
    ( Error(..)
    , InterpreterError(..)
    , LexerError(..)
    , ParserError(..)
    , toString
    )

import AST exposing (Id)


type Error
    = LexerError LexerError
    | ParserError ParserError
    | InterpreterError InterpreterError


type LexerError
    = TodoLexerError


type ParserError
    = TodoParserError


type InterpreterError
    = ExpectedChildNode String
    | VarNotFound Id
    | RootVarNotFound Id
    | ExpectedModule String
    | ExpectedParent


toString : Error -> String
toString error =
    case error of
        LexerError lexerError ->
            case lexerError of
                TodoLexerError ->
                    "TodoLexerError"

        ParserError parserError ->
            case parserError of
                TodoParserError ->
                    "TodoParserError"

        InterpreterError interpreterError ->
            case interpreterError of
                ExpectedChildNode node ->
                    "Expected child node " ++ node

                VarNotFound id ->
                    "Var not found: " ++ AST.idToString id

                RootVarNotFound id ->
                    "Root var not found: " ++ AST.idToString id

                ExpectedModule module_ ->
                    "Expected module " ++ module_

                ExpectedParent ->
                    "Expected parent"
