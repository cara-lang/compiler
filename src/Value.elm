module Value exposing (Value(..), toString)

import Id
import Intrinsic exposing (Intrinsic)


type Value
    = VInt Int
    | VUnit
    | VIntrinsic Intrinsic


toString : Value -> String
toString value =
    case value of
        VInt int ->
            String.fromInt int

        VUnit ->
            "()"

        VIntrinsic _ ->
            "<intrinsic>"
