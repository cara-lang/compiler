module Token exposing
    ( Token
    , Type(..)
    , getChar
    , getFloat
    , getInt
    , getLowerName
    , getQualifier
    , getString
    , getUpperName
    , isChar
    , isFloat
    , isInt
    , isLowerName
    , isQualifier
    , isString
    , isUpperName
    )

import Loc exposing (Loc)


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
    | True_
    | False_
    | Type
    | Alias
    | Module
    | Private
    | Opaque
    | Extend
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
