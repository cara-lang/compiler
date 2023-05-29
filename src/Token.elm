module Token exposing (Token, Type(..))

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
    | TTrue
    | TFalse
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
