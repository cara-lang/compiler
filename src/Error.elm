module Error exposing
    ( DesugarError(..)
    , Error(..)
    , InterpreterError(..)
    , LexerError(..)
    , ParserContext(..)
    , ParserError(..)
    , code
    , inspect
    , loc
    , title
    )

import AST.Frontend as F
import Id exposing (Id)
import Loc exposing (Loc)
import Operator exposing (BinaryOp, UnaryOp)
import Token
import Value exposing (Value)


type Error
    = LexerError ( Loc, LexerError )
    | ParserError ( Loc, ParserError, List ParserContext )
    | InterpreterError ( Loc, InterpreterError )
    | DesugarError ( Loc, DesugarError )


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
    | UnfinishedStringInterpolation
    | CouldntLexInsideStringInterpolation ( Loc, LexerError )
    | UnusedExpression F.Expr
    | IfWithoutElse { cond : F.Expr, then_ : F.Expr }
    | ExpectedSpecificFunctionName -- Shouldn't be visible to the user
    | BlockExprWithNoReturnExpr
    | UnexpectedIntrinsic


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
    | IfConditionNotBool
    | PatternMismatch
    | NoCaseBranchMatched
    | MultipleSpreadPatterns
    | PatternDidNotMatch ( F.Pattern, Value )
    | RecordFieldNotFound String
    | EffectfulStmtInPureBlock_
    | UnnecessaryBang
    | CallingNonFunction
    | CallingRecordGetterOnNonRecord
    | CallingRecordGetOnNonRecord
    | AccessingMissingTupleElement String
    | SpreadingNonRecord
    | EquatingNonequatable
    | UnknownBinaryOpOverload ( Value, BinaryOp, Value )
    | UnknownUnaryOpOverload ( UnaryOp, Value )
    | UnexpectedArgument Value
    | MainIsNotFunction -- TODO this should perhaps be a typecheck error


type DesugarError
    = DestructuringNonSingletonConstructor


type ParserContext
    = -- Declarations
      InTypeAliasDecl
    | InTypeDecl
    | InIntrinsicTypeDecl
    | InExtendModuleDecl
    | InModuleDecl
    | InTestDecl
    | InStmtDecl
      -- Statements
    | InStmt
    | InUseModuleStmt
    | InLetBangStmt
    | InLetStmt
    | InBangStmt
    | InFunctionDefStmt
    | InValueAnnotationStmt
    | InBinaryOperatorDefStmt
    | InUnaryOperatorDefStmt
    | InBinaryOperatorAnnotationStmt
    | InUnaryOperatorAnnotationStmt
      --
    | InExpr
      --
    | InPattern


title : Error -> String
title error =
    case error of
        LexerError ( _, lexerError ) ->
            case lexerError of
                NonterminatedChar ->
                    "Non-terminated character"

                NonterminatedString ->
                    "Non-terminated string"

                NonterminatedMultilineString ->
                    "Non-terminated multi-line string"

                UnfinishedBlockComment ->
                    "Unfinished block comment"

                EmptyChar ->
                    "Empty character"

                UnescapedTabInChar ->
                    "Unescaped tab in a character"

                UnescapedNewlineInChar ->
                    "Unescaped newline in a character"

                UnescapedNewlineInString ->
                    "Unescaped newline in a single-line string"

                UnexpectedEscapedCharacterInChar _ ->
                    "Unexpected escaped character in a character"

                UnexpectedEscapedCharacterInString _ ->
                    "Unexpected escaped character in a string"

                UnexpectedEscapedCharacterInMultilineString _ ->
                    "Unexpected escaped character in a multi-line string"

                ExpectedLowerName ->
                    "Expected lower name"

                ExpectedUpperName ->
                    "Expected upper name"

                ExpectedNumber ->
                    "Expected number"

                UnexpectedChar c ->
                    "Unexpected character: '{CHAR}'"
                        |> String.replace "{CHAR}" (String.fromChar c)

                HexIntStartedWith0X ->
                    "Hexadecimal integer started with 0X"

                BinaryIntStartedWith0X ->
                    "Binary integer started with 0B"

                OctalIntStartedWith0X ->
                    "Octal integer started with 0O"

                ShebangIsNotFirst ->
                    "Shebang comment is not first"

                FloatExpectedNumbersAfterE ->
                    "Float: expected numbers after E"

                FloatExpectedNumbersAfterDot ->
                    "Float: expected numbers after dot"

                UnexpectedBinaryIntCharacter c ->
                    "Binary integer: unexpected character '{C}'"
                        |> String.replace "{C}" (String.fromChar c)

                UnexpectedOctIntCharacter c ->
                    "Octal integer: unexpected character '{C}'"
                        |> String.replace "{C}" (String.fromChar c)

                UnexpectedHexIntCharacter c ->
                    "Hexadecimal integer: unexpected character '{C}'"
                        |> String.replace "{C}" (String.fromChar c)

                UnfinishedBinaryInt ->
                    "Unfinished binary integer"

                UnfinishedOctInt ->
                    "Unfinished octal integer"

                UnfinishedHexInt ->
                    "Unfinished hexadecimal integer"

                InvalidBinaryInt ->
                    "Invalid binary integer"

                InvalidOctInt ->
                    "Invalid octal integer"

                InvalidHexInt ->
                    "Invalid hexadecimal integer"

        ParserError ( _, parserError, _ ) ->
            case parserError of
                ExpectedNonemptyTokens ->
                    -- Shouldn't happen
                    "Expected nonempty tokens"

                CouldntMoveLeft ->
                    "Couldn't move the token list left"

                RanPastEndOfTokens ->
                    "Ran past end of tokens"

                CouldntGetTokenData ->
                    "Couldn't get token data"

                ExpectedToken t ->
                    "Expected token: " ++ Token.toString t

                AssignmentOfExprToUnderscore ->
                    "Assignment of expression to underscore"

                EmptyOneOf ->
                    "oneOf was given empty list of parsers to try"

                OneOfDidntMatchAnyCommited ->
                    "oneOf didn't match any commited path (and there were no noncommited paths)"

                MixedHoles ->
                    "Anonymous function shorthand with mixed holes"

                NonNumberedHole str ->
                    "(Should be impossible, we shouldn't allow _x names in user code) Non-numbered hole: " ++ str

                UnknownBinaryOp str ->
                    "Unknown binary op: " ++ str

                UnknownUnaryOp str ->
                    "Unknown unary op: " ++ str

                EffectfulStmtInPureBlock ->
                    "Effectful statement in a pure block"

                BinaryOpAnnotationNotFn2 ->
                    "Binary operation annotation was not a 2-arg function"

                UnaryOpAnnotationNotFn1 ->
                    "Unary operation annotation was not a 1-arg function"

                UnfinishedStringInterpolation ->
                    "Unfinished string interpolation"

                CouldntLexInsideStringInterpolation _ ->
                    "Couldn't lex inside string interpolation"

                UnusedExpression _ ->
                    "Unused expression"

                IfWithoutElse _ ->
                    "If expression without an else branch"

                ExpectedSpecificFunctionName ->
                    "Expected specific function name"

                BlockExprWithNoReturnExpr ->
                    "Block expression with no return expression"

                UnexpectedIntrinsic ->
                    "Unexpected `intrinsic`"

        InterpreterError ( _, interpreterError ) ->
            case interpreterError of
                VarNotFound id ->
                    "Unknown variable: `{ID}`"
                        |> String.replace "{ID}" (Id.toString id)

                RootVarNotFound id ->
                    "Root var not found: " ++ Id.toString id

                ExpectedModule module_ ->
                    "Expected module " ++ module_

                ExpectedParent ->
                    "Expected parent"

                UnexpectedArity ->
                    "Unexpected arity"

                TupleLengthMismatch { wanted, length } ->
                    "Tuple length mismatch: wanted to get {WHICH} field of a tuple that has only {LENGTH} items"
                        |> String.replace "{WHICH}" (String.fromInt wanted ++ th (wanted + 1))
                        |> String.replace "{LENGTH}" (String.fromInt length)

                IfConditionNotBool ->
                    "If expression with a non-bool condition"

                PatternMismatch ->
                    "Pattern mismatch"

                NoCaseBranchMatched ->
                    "No case..of branch matched"

                MultipleSpreadPatterns ->
                    "Multiple spreads in a list pattern"

                PatternDidNotMatch ( pattern, value ) ->
                    "Pattern did not match: ({PATTERN}, {VALUE})"
                        |> String.replace "{PATTERN}" (F.patternToString pattern)
                        |> String.replace "{VALUE}" (Value.toInspectString value)

                RecordFieldNotFound field ->
                    "Record field not found: " ++ field

                EffectfulStmtInPureBlock_ ->
                    "Bang used outside effect block"

                UnnecessaryBang ->
                    "Unnecessary bang"

                CallingNonFunction ->
                    "Calling a non-function"

                CallingRecordGetterOnNonRecord ->
                    "Calling a record getter on a non-record"

                CallingRecordGetOnNonRecord ->
                    "Trying to access a record field from a non-record"

                AccessingMissingTupleElement _ ->
                    "Trying to access a missing tuple element"

                SpreadingNonRecord ->
                    "Spreading a non-record"

                EquatingNonequatable ->
                    "Equating nonequatable"

                UnknownBinaryOpOverload ( left, op, right ) ->
                    "Unknown binary op: {LEFT}, {OP}, {RIGHT}"
                        |> String.replace "{LEFT}" (Value.toInspectString left)
                        |> String.replace "{OP}" (Operator.binaryOpToString op)
                        |> String.replace "{RIGHT}" (Value.toInspectString right)

                UnknownUnaryOpOverload ( op, arg ) ->
                    "Unknown unary op: {OP}, {ARG}"
                        |> String.replace "{OP}" (Operator.unaryOpToString op)
                        |> String.replace "{ARG}" (Value.toInspectString arg)

                UnexpectedArgument value ->
                    "Unexpected argument: " ++ Value.toInspectString value

                MainIsNotFunction ->
                    "Main is not a function"

        DesugarError ( _, desugarError ) ->
            case desugarError of
                DestructuringNonSingletonConstructor ->
                    "Destructuring a non-singleton constructor"


code : Error -> String
code error =
    case error of
        LexerError ( _, lexerError ) ->
            case lexerError of
                NonterminatedChar ->
                    "E0033"

                NonterminatedString ->
                    "E0034"

                NonterminatedMultilineString ->
                    "E0035"

                UnfinishedBlockComment ->
                    "E0009"

                EmptyChar ->
                    "E0019"

                UnescapedTabInChar ->
                    "E0018"

                UnescapedNewlineInChar ->
                    "E0017"

                UnescapedNewlineInString ->
                    "E0012"

                UnexpectedEscapedCharacterInChar _ ->
                    "E0028"

                UnexpectedEscapedCharacterInString _ ->
                    "E0014"

                UnexpectedEscapedCharacterInMultilineString _ ->
                    "E0029"

                ExpectedLowerName ->
                    -- TODO
                    "EXXXX"

                ExpectedUpperName ->
                    -- TODO
                    "EXXXX"

                ExpectedNumber ->
                    -- TODO
                    "EXXXX"

                UnexpectedChar c ->
                    -- TODO
                    "EXXXX"

                HexIntStartedWith0X ->
                    "E0024"

                BinaryIntStartedWith0X ->
                    "E0025"

                OctalIntStartedWith0X ->
                    "E0026"

                ShebangIsNotFirst ->
                    "E0015"

                FloatExpectedNumbersAfterE ->
                    -- TODO
                    "EXXXX"

                FloatExpectedNumbersAfterDot ->
                    -- TODO
                    "EXXXX"

                UnexpectedBinaryIntCharacter c ->
                    -- TODO
                    "EXXXX"

                UnexpectedOctIntCharacter c ->
                    -- TODO
                    "EXXXX"

                UnexpectedHexIntCharacter c ->
                    -- TODO
                    "EXXXX"

                UnfinishedBinaryInt ->
                    -- TODO
                    "EXXXX"

                UnfinishedOctInt ->
                    -- TODO
                    "EXXXX"

                UnfinishedHexInt ->
                    -- TODO
                    "EXXXX"

                InvalidBinaryInt ->
                    -- TODO
                    "EXXXX"

                InvalidOctInt ->
                    -- TODO
                    "EXXXX"

                InvalidHexInt ->
                    -- TODO
                    "EXXXX"

        ParserError ( _, parserError, _ ) ->
            case parserError of
                ExpectedNonemptyTokens ->
                    -- TODO
                    "EXXXX"

                CouldntMoveLeft ->
                    -- TODO
                    "EXXXX"

                RanPastEndOfTokens ->
                    -- TODO
                    "EXXXX"

                CouldntGetTokenData ->
                    -- TODO
                    "EXXXX"

                ExpectedToken t ->
                    -- TODO
                    "EXXXX"

                AssignmentOfExprToUnderscore ->
                    "E0013"

                EmptyOneOf ->
                    -- TODO
                    "EXXXX"

                OneOfDidntMatchAnyCommited ->
                    -- TODO
                    "EXXXX"

                MixedHoles ->
                    "E0020"

                NonNumberedHole str ->
                    -- TODO
                    "EXXXX"

                UnknownBinaryOp str ->
                    -- TODO
                    "EXXXX"

                UnknownUnaryOp str ->
                    -- TODO
                    "EXXXX"

                EffectfulStmtInPureBlock ->
                    -- TODO
                    "EXXXX"

                BinaryOpAnnotationNotFn2 ->
                    -- TODO
                    "EXXXX"

                UnaryOpAnnotationNotFn1 ->
                    -- TODO
                    "EXXXX"

                UnfinishedStringInterpolation ->
                    -- TODO
                    "EXXXX"

                CouldntLexInsideStringInterpolation _ ->
                    -- TODO
                    "EXXXX"

                UnusedExpression _ ->
                    "E0011"

                IfWithoutElse _ ->
                    "E0021"

                ExpectedSpecificFunctionName ->
                    -- TODO
                    "EXXXX"

                BlockExprWithNoReturnExpr ->
                    "E0032"

                UnexpectedIntrinsic ->
                    -- TODO
                    "EXXXX"

        InterpreterError ( _, interpreterError ) ->
            case interpreterError of
                VarNotFound id ->
                    "E0001"

                RootVarNotFound id ->
                    -- TODO
                    "EXXXX"

                ExpectedModule module_ ->
                    -- TODO
                    "EXXXX"

                ExpectedParent ->
                    -- TODO
                    "EXXXX"

                UnexpectedArity ->
                    -- TODO
                    "EXXXX"

                TupleLengthMismatch { wanted, length } ->
                    -- TODO
                    "EXXXX"

                IfConditionNotBool ->
                    "E0025"

                PatternMismatch ->
                    -- TODO
                    "EXXXX"

                NoCaseBranchMatched ->
                    -- TODO
                    "EXXXX"

                MultipleSpreadPatterns ->
                    "E0037"

                PatternDidNotMatch ( pattern, value ) ->
                    -- TODO
                    "EXXXX"

                RecordFieldNotFound field ->
                    "E0007"

                EffectfulStmtInPureBlock_ ->
                    "E0027"

                UnnecessaryBang ->
                    -- TODO
                    "EXXXX"

                CallingNonFunction ->
                    -- TODO
                    "EXXXX"

                CallingRecordGetterOnNonRecord ->
                    -- TODO
                    "EXXXX"

                CallingRecordGetOnNonRecord ->
                    "E0022"

                AccessingMissingTupleElement _ ->
                    "E0023"

                SpreadingNonRecord ->
                    -- TODO
                    "EXXXX"

                EquatingNonequatable ->
                    -- TODO
                    "EXXXX"

                UnknownBinaryOpOverload ( left, op, right ) ->
                    -- TODO
                    "EXXXX"

                UnknownUnaryOpOverload ( op, arg ) ->
                    -- TODO
                    "EXXXX"

                UnexpectedArgument value ->
                    -- TODO
                    "EXXXX"

                MainIsNotFunction ->
                    "E0036"

        DesugarError ( _, desugarError ) ->
            case desugarError of
                DestructuringNonSingletonConstructor ->
                    -- TODO
                    "EXXXX"


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


loc : Error -> Loc
loc error =
    case error of
        LexerError ( loc_, _ ) ->
            loc_

        ParserError ( loc_, _, _ ) ->
            loc_

        InterpreterError ( loc_, _ ) ->
            loc_

        DesugarError ( loc_, _ ) ->
            loc_


inspect : Error -> String
inspect error =
    case error of
        LexerError ( _, err ) ->
            "LexerError: " ++ Debug.toString err

        ParserError ( _, err, context ) ->
            "ParserError: {ERR} (context: {CONTEXT})"
                |> String.replace "{ERR}" (Debug.toString err)
                |> String.replace "{CONTEXT}" (Debug.toString (List.reverse context))

        InterpreterError ( _, err ) ->
            "InterpreterError: " ++ Debug.toString err

        DesugarError ( _, err ) ->
            "DesugarError: " ++ Debug.toString err
