module Error exposing
    ( Error(..)
    , InterpreterError(..)
    , LexerError(..)
    , ParserError(..)
    , title
    )

import AST exposing (Pattern)
import Id exposing (Id)
import Loc exposing (Loc)
import Token
import Value exposing (Value)


type Error
    = LexerError ( Loc, LexerError )
    | ParserError ( Loc, ParserError )
    | InterpreterError InterpreterError


type LexerError
    = NonterminatedChar
    | NonterminatedString
    | NonterminatedMultilineString
    | UnfinishedBlockComment
    | EmptyChar
    | UnescapedTabInChar
    | UnescapedNewlineInChar
    | UnescapedNewlineInString
    | UnexpectedEscapedCharacterInChar Char
    | UnexpectedEscapedCharacterInString Char
    | UnexpectedEscapedCharacterInMultilineString Char
    | ExpectedLowerName
    | ExpectedUpperName
    | ExpectedNumber
    | UnexpectedChar Char
    | HexIntStartedWith0X
    | BinaryIntStartedWith0X
    | OctalIntStartedWith0X
    | ShebangIsNotFirst
    | FloatExpectedNumbersAfterE
    | FloatExpectedNumbersAfterDot
    | UnexpectedBinaryIntCharacter Char
    | UnexpectedOctIntCharacter Char
    | UnexpectedHexIntCharacter Char
    | UnfinishedBinaryInt
    | UnfinishedOctInt
    | UnfinishedHexInt
    | InvalidBinaryInt
    | InvalidOctInt
    | InvalidHexInt


type ParserError
    = ExpectedNonemptyTokens
    | CouldntMoveLeft
    | RanPastEndOfTokens
    | CouldntGetTokenData
    | ExpectedToken Token.Type
    | AssignmentOfExprToUnderscore
    | EmptyOneOf
    | OneOfDidntMatchAnyCommited -- in case where there are no noncommited
    | MixedHoles
    | NonNumberedHole String
    | UnknownBinaryOp String
    | UnknownUnaryOp String
    | EffectfulStmtInPureBlock
    | BinaryOpAnnotationNotFn2
    | UnaryOpAnnotationNotFn1


type InterpreterError
    = VarNotFound Id
    | RootVarNotFound Id
    | ExpectedModule String
    | ExpectedParent
    | UnexpectedArity
    | TupleLengthMismatch
        { wanted : Int -- 0-based
        , length : Int
        }
    | TupleUnknownField String
    | IfConditionNotBool
    | PatternMismatch
    | NoCaseBranchMatched
    | MultipleSpreadPatterns
    | PatternDidNotMatch ( Pattern, Value )
    | RecordFieldNotFound String
    | EffectfulStmtInPureBlock_
    | UnnecessaryBang
    | CallingNonFunction


title : Error -> String
title error =
    case error of
        LexerError ( loc, lexerError ) ->
            "Lexer error at "
                ++ Loc.toString loc
                ++ ": "
                ++ (case lexerError of
                        NonterminatedChar ->
                            -- TODO error code
                            "EXXXX: Nonterminated character"

                        NonterminatedString ->
                            -- TODO error code
                            "EXXXX: Nonterminated string"

                        NonterminatedMultilineString ->
                            -- TODO error code
                            "EXXXX: Nonterminated multiline string"

                        UnfinishedBlockComment ->
                            "E0009: Unfinished block comment"

                        EmptyChar ->
                            "E0019: Empty character"

                        UnescapedTabInChar ->
                            "E0018: Unescaped tab in a character"

                        UnescapedNewlineInChar ->
                            -- TODO error code
                            "EXXXX: Unescaped newline in a character"

                        UnescapedNewlineInString ->
                            -- TODO error code
                            "EXXXX: Unescaped newline in a string"

                        UnexpectedEscapedCharacterInChar _ ->
                            "E0028: Unexpected escaped character in a character"

                        UnexpectedEscapedCharacterInString _ ->
                            "E0014: Unexpected escaped character in a string"

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

                        ShebangIsNotFirst ->
                            "E0015: Shebang comment is not first"

                        FloatExpectedNumbersAfterE ->
                            -- TODO error code
                            "Float: expected numbers after E"

                        FloatExpectedNumbersAfterDot ->
                            -- TODO error code
                            "Float: expected numbers after dot"

                        UnexpectedBinaryIntCharacter c ->
                            -- TODO error code
                            "Binary integer: unexpected character '{C}'"
                                |> String.replace "{C}" (String.fromChar c)

                        UnexpectedOctIntCharacter c ->
                            -- TODO error code
                            "Octal integer: unexpected character '{C}'"
                                |> String.replace "{C}" (String.fromChar c)

                        UnexpectedHexIntCharacter c ->
                            -- TODO error code
                            "Hexadecimal integer: unexpected character '{C}'"
                                |> String.replace "{C}" (String.fromChar c)

                        UnfinishedBinaryInt ->
                            -- TODO error code
                            "Unfinished binary integer"

                        UnfinishedOctInt ->
                            -- TODO error code
                            "Unfinished octal integer"

                        UnfinishedHexInt ->
                            -- TODO error code
                            "Unfinished hexadecimal integer"

                        InvalidBinaryInt ->
                            -- TODO error code
                            "Invalid binary integer"

                        InvalidOctInt ->
                            -- TODO error code
                            "Invalid octal integer"

                        InvalidHexInt ->
                            -- TODO error code
                            "Invalid hexadecimal integer"
                   )

        ParserError ( loc, parserError ) ->
            "Parser error at "
                ++ Loc.toString loc
                ++ ": "
                ++ (case parserError of
                        ExpectedNonemptyTokens ->
                            -- TODO error code
                            -- Shouldn't happen
                            "Expected nonempty tokens"

                        CouldntMoveLeft ->
                            -- TODO error code
                            "Couldn't move the token list left"

                        RanPastEndOfTokens ->
                            -- TODO error code
                            "Ran past end of tokens"

                        CouldntGetTokenData ->
                            -- TODO error code
                            "Couldn't get token data"

                        ExpectedToken t ->
                            -- TODO error code
                            "Expected token: " ++ Debug.toString t

                        AssignmentOfExprToUnderscore ->
                            "E0013: Assignment of expression to underscore"

                        EmptyOneOf ->
                            -- TODO error code
                            "oneOf was given empty list of parsers to try"

                        OneOfDidntMatchAnyCommited ->
                            -- TODO error code
                            "oneOf didn't match any commited path (and there were no noncommited paths)"

                        MixedHoles ->
                            -- TODO error code
                            "Mixed holes"

                        NonNumberedHole str ->
                            -- TODO error code
                            "(Should be impossible, we shouldn't allow _x names in user code) Non-numbered hole: " ++ str

                        UnknownBinaryOp str ->
                            -- TODO error code
                            "Unknown binary op: " ++ str

                        UnknownUnaryOp str ->
                            -- TODO error code
                            "Unknown unary op: " ++ str

                        EffectfulStmtInPureBlock ->
                            -- TODO error code
                            "Effectful statement in a pure block"

                        BinaryOpAnnotationNotFn2 ->
                            -- TODO error code
                            "Binary operation annotation was not a 2-arg function"

                        UnaryOpAnnotationNotFn1 ->
                            -- TODO error code
                            "Unary operation annotation was not a 1-arg function"
                   )

        InterpreterError interpreterError ->
            "Interpreter error: "
                ++ (case interpreterError of
                        VarNotFound id ->
                            -- TODO error code
                            "Var not found: " ++ Id.toString id

                        RootVarNotFound id ->
                            -- TODO error code
                            "Root var not found: " ++ Id.toString id

                        ExpectedModule module_ ->
                            -- TODO error code
                            "Expected module " ++ module_

                        ExpectedParent ->
                            -- TODO error code
                            "Expected parent"

                        UnexpectedArity ->
                            -- TODO error code
                            "Unexpected arity"

                        TupleLengthMismatch { wanted, length } ->
                            -- TODO error code
                            "Tuple length mismatch: wanted to get {WHICH} field of a tuple that has only {LENGTH} items"
                                |> String.replace "{WHICH}" (String.fromInt wanted ++ th (wanted + 1))
                                |> String.replace "{LENGTH}" (String.fromInt length)

                        TupleUnknownField field ->
                            -- TODO error code
                            "Tried to access field '{FIELD}' on a tuple"
                                |> String.replace "{FIELD}" field

                        IfConditionNotBool ->
                            -- TODO error code
                            "If condition not bool"

                        PatternMismatch ->
                            -- TODO error code
                            "Pattern mismatch"

                        NoCaseBranchMatched ->
                            -- TODO error code
                            "No case..of branch matched"

                        MultipleSpreadPatterns ->
                            -- TODO error code
                            "Multiple spread patterns in a list pattern"

                        PatternDidNotMatch ( pattern, value ) ->
                            -- TODO error code
                            "Pattern did not match: " ++ Debug.toString ( pattern, value )

                        RecordFieldNotFound field ->
                            -- TODO error code
                            "Record field not found: " ++ field

                        EffectfulStmtInPureBlock_ ->
                            -- TODO error code
                            "Effectful statement in a pure block"

                        UnnecessaryBang ->
                            -- TODO error code
                            "Unnecessary bang"

                        CallingNonFunction ->
                            -- TODO error code
                            "Calling a non-function"
                   )


th : Int -> String
th n =
    case n of
        1 ->
            "st"

        2 ->
            "nd"

        3 ->
            "rd"

        _ ->
            "th"
