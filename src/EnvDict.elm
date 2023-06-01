module EnvDict exposing
    ( EnvDict
    , empty
    , singleton
    , toList
    )

import Dict.Any exposing (AnyDict)
import Id exposing (Id)
import Value exposing (Value)


type alias EnvDict =
    AnyDict String Id Value


toComparable : Id -> String
toComparable id =
    Id.toString id


empty : EnvDict
empty =
    Dict.Any.empty toComparable


singleton : Id -> Value -> EnvDict
singleton id value =
    Dict.Any.singleton id value toComparable


toList : EnvDict -> List ( Id, Value )
toList dict =
    Dict.Any.toList dict
