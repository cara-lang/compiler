module Debug.Extra exposing (logged1, logged2)


logged1 : String -> (a -> b) -> (a -> b)
logged1 label fn a =
    let
        _ =
            Debug.log (label ++ " - IN") a
    in
    fn a
        |> Debug.log (label ++ " - OUT")


logged2 : String -> (a -> b -> c) -> (a -> b -> c)
logged2 label fn a b =
    let
        _ =
            Debug.log (label ++ " - IN") ( a, b )
    in
    fn a b
        |> Debug.log (label ++ " - OUT")
