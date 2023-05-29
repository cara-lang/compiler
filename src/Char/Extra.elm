module Char.Extra exposing (digitToInt)

{-| -}


{-| Only makes sense in the domain of '0'..'9'
-}
digitToInt : Char -> Int
digitToInt c =
    Char.toCode c - 48
