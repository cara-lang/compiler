module Value exposing (Value(..), toString)

import Intrinsic exposing (Intrinsic)


type Value
    = VInt Int
    | VFloat Float
    | VString String
    | VUnit
    | VIntrinsic Intrinsic
    | VList (List Value)
    | VTuple (List Value)
    | VRecordGetter String


toString : Value -> String
toString value =
    case value of
        VInt int ->
            String.fromInt int

        VFloat float ->
            String.fromFloat float

        VString str ->
            -- TODO quoting?
            -- TODO escaping?
            str

        VUnit ->
            "()"

        VIntrinsic _ ->
            "<intrinsic>"

        VList values ->
            "[" ++ String.join "," (List.map toString values) ++ "]"

        VTuple values ->
            "(" ++ String.join "," (List.map toString values) ++ ")"

        VRecordGetter field ->
            "<record getter .{FIELD}>"
                |> String.replace "{FIELD}" field
