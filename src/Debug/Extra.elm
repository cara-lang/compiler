module Debug.Extra exposing
    ( logged1
    , logged2
    , prettyPrint
    , standOutErr
    , standOutInfo
    , todo1
    )

import Console
import DebugParser


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


prettyPrint : a -> String
prettyPrint a =
    let
        origStr =
            Debug.toString a
    in
    origStr
        |> DebugParser.parseValue prettyPrintConfig
        |> Result.withDefault origStr


prettyPrintConfig : DebugParser.Config String
prettyPrintConfig =
    { bool =
        \bool ->
            if bool then
                "True"

            else
                "False"
    , string =
        \str ->
            -- TODO escaping
            "\"" ++ str ++ "\""
    , char = \c -> "'" ++ String.fromChar c ++ "'"
    , number = \float -> String.fromFloat float
    , function = "<function>"
    , internals = "<internals>"
    , unit = "()"
    , bytes = \bytes -> "<bytes: " ++ String.fromInt bytes ++ ">"
    , file = \file -> "<file: " ++ file ++ ">"
    , list = prettyPrintListlike "[" "]"
    , array = prettyPrintListlike "Array[" "]"
    , set = prettyPrintListlike "Set[" "]"
    , tuple = prettyPrintListlike "(" ")"
    , customType =
        \ct data ->
            ct
                ++ String.join ""
                    (List.map
                        (\s -> "\n  - " ++ indentTail (indentTail s))
                        data
                    )
    , record =
        \fields ->
            prettyPrintListlike
                "{"
                "}"
                (List.map
                    (\( field, value ) -> field ++ " = " ++ value)
                    fields
                )
    , dict =
        \kvs ->
            prettyPrintListlike
                "Dict["
                "]"
                (List.map (\( k, v ) -> k ++ ": " ++ v) kvs)
    }


indentTail : String -> String
indentTail str =
    str
        |> String.lines
        |> List.indexedMap
            (\i s ->
                if i == 0 then
                    s

                else
                    "  " ++ s
            )
        |> String.join "\n"


prettyPrintListlike : String -> String -> List String -> String
prettyPrintListlike opening closing list =
    if List.isEmpty list then
        opening ++ closing

    else
        opening
            ++ "\n"
            ++ String.join "\n"
                (List.map
                    (\s -> "  " ++ indentTail s ++ ",")
                    list
                )
            ++ "\n"
            ++ closing


todo1 : String -> a -> b
todo1 msg val =
    Debug.todo (standOutErr (msg ++ ": " ++ prettyPrint val))


standOutErr : String -> String
standOutErr str =
    str
        |> Console.bold
        |> Console.bgRed
        |> Console.white


standOutInfo : String -> String
standOutInfo str =
    str
        |> Console.bold
        |> Console.bgBlue
        |> Console.white
