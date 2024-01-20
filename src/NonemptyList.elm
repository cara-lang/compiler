module NonemptyList exposing
    ( NonemptyList
    , fromCons
    , fromList
    , singleton
    , toList
    )


type alias NonemptyList a =
    ( a, List a )


singleton : a -> NonemptyList a
singleton x =
    ( x, [] )


fromCons : a -> List a -> NonemptyList a
fromCons x xs =
    ( x, xs )


toList : NonemptyList a -> List a
toList ( x, xs ) =
    x :: xs


fromList : List a -> Maybe (NonemptyList a)
fromList xs =
    case xs of
        [] ->
            Nothing

        fst :: rest ->
            Just ( fst, rest )
