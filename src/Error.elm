module Error exposing
    ( Error(..)
    , InterpreterError(..)
    , LexerError(..)
    , ParserError(..)
    , title
    )

import AST exposing (Id)


type Error
    = LexerError LexerError
    | ParserError ParserError
    | InterpreterError InterpreterError


type LexerError
    = NonterminatedChar
    | NonterminatedMultilineString
    | EmptyChar
    | UnescapedTabInChar
    | UnexpectedEscapedCharacterInChar Char
    | UnexpectedEscapedCharacterInMultilineString Char
    | ExpectedLowerName
    | ExpectedUpperName
    | ExpectedNumber
    | UnexpectedChar Char
    | HexIntStartedWith0X
    | BinaryIntStartedWith0X
    | OctalIntStartedWith0X


type ParserError
    = TodoParserError


type InterpreterError
    = ExpectedChildNode String
    | VarNotFound Id
    | RootVarNotFound Id
    | ExpectedModule String
    | ExpectedParent


title : Error -> String
title error =
    case error of
        LexerError lexerError ->
            case lexerError of
                NonterminatedChar ->
                    -- TODO error code
                    "EXXXX: Nonterminated character"

                NonterminatedMultilineString ->
                    -- TODO error code
                    "EXXXX: Nonterminated multiline string"

                EmptyChar ->
                    "E0019: Empty character"

                UnescapedTabInChar ->
                    "E0018: Unescaped tab in a character"

                UnexpectedEscapedCharacterInChar _ ->
                    "E0028: Unexpected escaped character in a character"

                UnexpectedEscapedCharacterInMultilineString _ ->
                    "E0029: Unexpected escaped character in a multi-line string"

                ExpectedLowerName ->
                    -- TODO error code
                    "Expected lower name"

                ExpectedUpperName ->
                    -- TODO error code
                    "Expected upper name"

                ExpectedNumber ->
                    -- TODO error code
                    "Expected number"

                UnexpectedChar c ->
                    -- TODO error code
                    "Unexpected character: '{CHAR}'"
                        |> String.replace "{CHAR}" (String.fromChar c)

                HexIntStartedWith0X ->
                    "E0024: Hexadecimal integer started with 0X"

                BinaryIntStartedWith0X ->
                    "E0025: Binary integer started with 0B"

                OctalIntStartedWith0X ->
                    "E0026: Octal integer started with 0O"

        ParserError parserError ->
            case parserError of
                TodoParserError ->
                    "TodoParserError"

        InterpreterError interpreterError ->
            case interpreterError of
                ExpectedChildNode node ->
                    -- TODO error code
                    "Expected child node " ++ node

                VarNotFound id ->
                    -- TODO error code
                    "Var not found: " ++ AST.idToString id

                RootVarNotFound id ->
                    -- TODO error code
                    "Root var not found: " ++ AST.idToString id

                ExpectedModule module_ ->
                    -- TODO error code
                    "Expected module " ++ module_

                ExpectedParent ->
                    -- TODO error code
                    "Expected parent"
