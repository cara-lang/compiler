module EnvDict exposing
    ( EnvDict
    , empty
    , fromList
    , singleton
    , toList
    )

{-| The typevar in `EnvDict value`is present to remove an import cycle.
It's meant to be used with Value.Value.
-}

import Dict.Any exposing (AnyDict)
import Id exposing (Id)


type alias EnvDict value =
    AnyDict String Id value


toComparable : Id -> String
toComparable id =
    Id.toString id


empty : EnvDict value
empty =
    Dict.Any.empty toComparable


singleton : Id -> value -> EnvDict value
singleton id value =
    Dict.Any.singleton id value toComparable


fromList : List ( Id, value ) -> EnvDict value
fromList list =
    Dict.Any.fromList toComparable list


toList : EnvDict value -> List ( Id, value )
toList dict =
    Dict.Any.toList dict
