module Value exposing (Value(..), toString)


type Value
    = VInt Int
    | VUnit


toString : Value -> String
toString value =
    case value of
        VInt int ->
            String.fromInt int

        VUnit ->
            "()"
