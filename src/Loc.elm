module Loc exposing (Loc, compare, toString, zero)


type alias Loc =
    { row : Int -- 1-based
    , col : Int -- 1-based
    }


{-| This doesn't happen organically - we only use it when there are _no_ tokens
in the file (this shouldn't happen as we always add EOF).
-}
zero : Loc
zero =
    { row = 0
    , col = 0
    }


compare : Loc -> Loc -> Order
compare locA locB =
    Basics.compare
        ( locA.row, locA.col )
        ( locB.row, locB.col )


toString : Loc -> String
toString loc =
    String.fromInt loc.row ++ ":" ++ String.fromInt loc.col
