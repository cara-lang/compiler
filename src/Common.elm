module Common exposing (tupleIndexToNumericField)


tupleIndexToNumericField : Int -> String
tupleIndexToNumericField n =
    "el" ++ String.fromInt n
