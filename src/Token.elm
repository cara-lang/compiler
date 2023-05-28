module Token exposing (Token, Type(..))


type alias Token =
    { type_ : Type

    -- TODO other info
    }


type Type
    = TodoTokenType
