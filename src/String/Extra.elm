module String.Extra exposing (at, indent)


indent : String -> String
indent str =
    str
        |> String.lines
        |> List.map (\s -> "    " ++ s)
        |> String.join "\n"


at : Int -> String -> Maybe Char
at n string =
    string
        |> String.slice n (n + 1)
        |> String.uncons
        |> Maybe.map Tuple.first
