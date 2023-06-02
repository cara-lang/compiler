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
    = Int_ Int
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
        Int_ _ ->
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


getInt : Type -> Maybe Int
getInt type_ =
    case type_ of
        Int_ n ->
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
        Char c ->
            Just c

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
        Getter g ->
            Just g

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