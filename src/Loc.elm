module Loc exposing (Loc)


type alias Loc =
    { row : Int -- 1-based
    , col : Int -- 1-based
    }
