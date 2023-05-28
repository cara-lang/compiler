module String.Extra exposing (indent)


indent : String -> String
indent str =
    str
        |> String.lines
        |> List.map (\s -> "    " ++ s)
        |> String.join "\n"
