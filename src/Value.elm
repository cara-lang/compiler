module Value exposing (Value(..), toString)

import Id
import Intrinsic exposing (Intrinsic)


type Value
    = VInt Int
    | VFloat Float
    | VUnit
    | VIntrinsic Intrinsic
    | VList (List Value)


toString : Value -> String
toString value =
    case value of
        VInt int ->
            String.fromInt int

        VFloat float ->
            String.fromFloat float

        VUnit ->
            "()"

        VIntrinsic _ ->
            "<intrinsic>"

        VList values ->
            "[" ++ String.join "," (List.map toString values) ++ "]"
