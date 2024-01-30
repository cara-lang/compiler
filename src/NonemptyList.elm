module NonemptyList exposing
    ( NonemptyList
    , fromCons
    , fromList
    , head
    , map
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


head : NonemptyList a -> a
head ( x, _ ) =
    x


map : (a -> b) -> NonemptyList a -> NonemptyList b
map f ( x, xs ) =
    ( f x, List.map f xs )
