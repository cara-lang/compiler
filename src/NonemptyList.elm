module NonemptyList exposing
    ( NonemptyList
    , fromCons
    , toList
    )


type alias NonemptyList a =
    ( a, List a )


fromCons : a -> List a -> NonemptyList a
fromCons x xs =
    ( x, xs )


toList : NonemptyList a -> List a
toList ( x, xs ) =
    x :: xs
