module Token exposing
    ( Token
    , Type(..)
    , getBacktickString
    , getChar
    , getFloat
    , getGetter
    , getHole
    , getInt
    , getLowerName
    , getOperator
    , getQualifier
    , getString
    , getUpperName
    , isBacktickString
    , isChar
    , isFloat
    , isGetter
    , isHole
    , isInt
    , isLowerName
    , isOperator
    , isQualifier
    , isString
    , isUpperName
    , toDebugString
    , toString
    )

import Loc exposing (Loc)
import Operator exposing (BinaryOp(..), Operator(..), UnaryOp(..))


type alias Token =
    { type_ : Type
    , loc : Loc
    }


type Type
    = Int Int
    | Float Float
    | Char String
    | String String
    | BacktickString String
    | Getter String
    | Qualifier String
    | LowerName String
    | UpperName String
    | Hole Int
    | Plus
    | Minus
    | Times
    | Div
    | Percent
    | Power
    | PlusPlus
    | Shl
    | Shr
    | Shru
    | Caret
    | AndAnd
    | And
    | OrOr
    | Lte
    | Lt
    | EqEq
    | Neq
    | Gt
    | Gte
    | Eq
    | DotDot
    | DotDotDot
    | Tilde
    | Case
    | Of
    | If
    | Then
    | Else
    | Use
    | True_
    | False_
    | Type
    | Alias
    | Module
    | Private
    | Opaque
    | Extend
    | Intrinsic
    | Backslash
    | Arrow
    | Underscore
    | LHole
    | LParen
    | RParen
    | LBrace
    | RBrace
    | LBracket
    | RBracket
    | Pipeline
    | Comma
    | ColonColon
    | Colon
    | Bang
    | Pipe
    | Test
    | With
    | EOL
    | EOF


isInt : Type -> Bool
isInt type_ =
    case type_ of
        Int _ ->
            True

        _ ->
            False


isFloat : Type -> Bool
isFloat type_ =
    case type_ of
        Float _ ->
            True

        _ ->
            False


isChar : Type -> Bool
isChar type_ =
    case type_ of
        Char _ ->
            True

        _ ->
            False


isString : Type -> Bool
isString type_ =
    case type_ of
        String _ ->
            True

        _ ->
            False


isBacktickString : Type -> Bool
isBacktickString type_ =
    case type_ of
        BacktickString _ ->
            True

        _ ->
            False


isGetter : Type -> Bool
isGetter type_ =
    case type_ of
        Getter _ ->
            True

        _ ->
            False


isQualifier : Type -> Bool
isQualifier type_ =
    case type_ of
        Qualifier _ ->
            True

        _ ->
            False


isLowerName : Type -> Bool
isLowerName type_ =
    case type_ of
        LowerName _ ->
            True

        _ ->
            False


isUpperName : Type -> Bool
isUpperName type_ =
    case type_ of
        UpperName _ ->
            True

        _ ->
            False


isHole : Type -> Bool
isHole type_ =
    case type_ of
        Hole _ ->
            True

        _ ->
            False


isOperator : Type -> Bool
isOperator type_ =
    case type_ of
        -- unary
        DotDot ->
            True

        Minus ->
            True

        Bang ->
            True

        -- binary
        AndAnd ->
            True

        OrOr ->
            True

        PlusPlus ->
            True

        DotDotDot ->
            True

        Pipe ->
            True

        Caret ->
            True

        And ->
            True

        EqEq ->
            True

        Neq ->
            True

        Lte ->
            True

        Lt ->
            True

        Gt ->
            True

        Gte ->
            True

        Shl ->
            True

        Shr ->
            True

        Shru ->
            True

        Plus ->
            True

        Times ->
            True

        Div ->
            True

        Percent ->
            True

        Power ->
            True

        _ ->
            False


{-| It's probably fine that we're choosing arbitrarily between Unary and Binary
for the operators that share lexemes (eg. Binary Minus and Unary NegateNum)
-}
getOperator : Type -> Maybe Operator
getOperator type_ =
    case type_ of
        -- unary
        DotDot ->
            Just (Unary InfiniteRange)

        Minus ->
            Just (Unary NegateNum)

        Bang ->
            Just (Unary NegateBool)

        -- binary
        AndAnd ->
            Just (Binary AndBool)

        OrOr ->
            Just (Binary OrBool)

        PlusPlus ->
            Just (Binary Append)

        DotDotDot ->
            Just (Binary RangeExclusive)

        Pipe ->
            Just (Binary OrBin)

        Caret ->
            Just (Binary XorBin)

        And ->
            Just (Binary AndBin)

        EqEq ->
            Just (Binary Operator.Eq)

        Neq ->
            Just (Binary Operator.Neq)

        Lte ->
            Just (Binary Operator.Lte)

        Lt ->
            Just (Binary Operator.Lt)

        Gt ->
            Just (Binary Operator.Gt)

        Gte ->
            Just (Binary Operator.Gte)

        Shl ->
            Just (Binary ShiftL)

        Shr ->
            Just (Binary ShiftR)

        Shru ->
            Just (Binary ShiftRU)

        Plus ->
            Just (Binary Operator.Plus)

        Times ->
            Just (Binary Operator.Times)

        Div ->
            Just (Binary Operator.Div)

        Percent ->
            Just (Binary Modulo)

        Power ->
            Just (Binary Operator.Power)

        _ ->
            Nothing


getInt : Type -> Maybe Int
getInt type_ =
    case type_ of
        Int n ->
            Just n

        _ ->
            Nothing


getFloat : Type -> Maybe Float
getFloat type_ =
    case type_ of
        Float n ->
            Just n

        _ ->
            Nothing


getChar : Type -> Maybe String
getChar type_ =
    case type_ of
        Char s ->
            Just s

        _ ->
            Nothing


getString : Type -> Maybe String
getString type_ =
    case type_ of
        String s ->
            Just s

        _ ->
            Nothing


getBacktickString : Type -> Maybe String
getBacktickString type_ =
    case type_ of
        BacktickString s ->
            Just s

        _ ->
            Nothing


getGetter : Type -> Maybe String
getGetter type_ =
    case type_ of
        Getter s ->
            Just s

        _ ->
            Nothing


getQualifier : Type -> Maybe String
getQualifier type_ =
    case type_ of
        Qualifier q ->
            Just q

        _ ->
            Nothing


getLowerName : Type -> Maybe String
getLowerName type_ =
    case type_ of
        LowerName n ->
            Just n

        _ ->
            Nothing


getUpperName : Type -> Maybe String
getUpperName type_ =
    case type_ of
        UpperName n ->
            Just n

        _ ->
            Nothing


getHole : Type -> Maybe Int
getHole type_ =
    case type_ of
        Hole n ->
            Just n

        _ ->
            Nothing


toDebugString : Token -> String
toDebugString t =
    "({LOC}) {TYPE}"
        |> String.replace "{LOC}" (Loc.toString t.loc)
        |> String.replace "{TYPE}" (toString t.type_)


toString : Type -> String
toString t =
    case t of
        Int n ->
            "Int " ++ String.fromInt n

        Float n ->
            "Float " ++ String.fromFloat n

        Char c ->
            "Char " ++ c

        String s ->
            "String " ++ s

        BacktickString s ->
            "BacktickString " ++ s

        Getter s ->
            "Getter " ++ s

        Qualifier s ->
            "Qualifier " ++ s

        LowerName s ->
            "LowerName " ++ s

        UpperName s ->
            "UpperName " ++ s

        Hole n ->
            "Hole " ++ String.fromInt n

        Plus ->
            "Plus"

        Minus ->
            "Minus"

        Times ->
            "Times"

        Div ->
            "Div"

        Percent ->
            "Percent"

        Power ->
            "Power"

        PlusPlus ->
            "PlusPlus"

        Shl ->
            "Shl"

        Shr ->
            "Shr"

        Shru ->
            "Shru"

        Caret ->
            "Caret"

        AndAnd ->
            "AndAnd"

        And ->
            "And"

        OrOr ->
            "OrOr"

        Lte ->
            "Lte"

        Lt ->
            "Lt"

        EqEq ->
            "EqEq"

        Neq ->
            "Neq"

        Gt ->
            "Gt"

        Gte ->
            "Gte"

        Eq ->
            "Eq"

        DotDot ->
            "DotDot"

        DotDotDot ->
            "DotDotDot"

        Tilde ->
            "Tilde"

        Case ->
            "Case"

        Of ->
            "Of"

        If ->
            "If"

        Then ->
            "Then"

        Else ->
            "Else"

        Use ->
            "Use"

        True_ ->
            "True_"

        False_ ->
            "False_"

        Type ->
            "Type"

        Alias ->
            "Alias"

        Module ->
            "Module"

        Private ->
            "Private"

        Opaque ->
            "Opaque"

        Extend ->
            "Extend"

        Intrinsic ->
            "Intrinsic"

        Backslash ->
            "Backslash"

        Arrow ->
            "Arrow"

        Underscore ->
            "Underscore"

        LHole ->
            "LHole"

        LParen ->
            "LParen"

        RParen ->
            "RParen"

        LBrace ->
            "LBrace"

        RBrace ->
            "RBrace"

        LBracket ->
            "LBracket"

        RBracket ->
            "RBracket"

        Pipeline ->
            "Pipeline"

        Comma ->
            "Comma"

        ColonColon ->
            "ColonColon"

        Colon ->
            "Colon"

        Bang ->
            "Bang"

        Pipe ->
            "Pipe"

        Test ->
            "Test"

        With ->
            "With"

        EOL ->
            "EOL"

        EOF ->
            "EOF"
