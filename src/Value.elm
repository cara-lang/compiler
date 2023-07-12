module Value exposing
    ( Value(..)
    , closureToString
    , isEqualityAllowed
    , just
    , nothing
    , toInspectString
    , toShowString
    )

import AST exposing (Expr, Pattern)
import Dict exposing (Dict)
import Env exposing (Env)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)


type Value
    = VInt Int
    | VFloat Float
    | VString String
    | VChar String
    | VBool Bool
    | VUnit
    | VIntrinsic Intrinsic
    | VList (List Value)
    | VTuple (List Value)
    | VRecord (Dict String Value)
    | VRecordGetter String
    | VConstructor { id : Id, args : List Value }
    | VClosure { args : List Pattern, body : Expr, env : Env Value }
    | VIo Value


toShowString : Value -> String
toShowString value =
    case value of
        VInt int ->
            String.fromInt int

        VFloat float ->
            String.fromFloat float

        VString str ->
            -- TODO quoting?
            -- TODO escaping?
            str

        VChar str ->
            -- TODO quoting?
            -- TODO escaping?
            str

        VBool bool ->
            if bool then
                "True"

            else
                "False"

        VUnit ->
            "()"

        VIntrinsic intrinsic ->
            "<intrinsic {ID}>"
                |> String.replace "{ID}" (Id.toString (Intrinsic.id intrinsic))

        VList values ->
            "[" ++ String.join "," (List.map toShowString values) ++ "]"

        VTuple values ->
            "(" ++ String.join "," (List.map toShowString values) ++ ")"

        VRecord fields ->
            "{"
                ++ (fields
                        |> Dict.toList
                        |> List.map (\( field, value_ ) -> field ++ ":" ++ toShowString value_)
                        |> String.join ","
                   )
                ++ "}"

        VRecordGetter field ->
            "<record getter .{FIELD}>"
                |> String.replace "{FIELD}" field

        VConstructor { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "(" ++ String.join "," (List.map toShowString args) ++ ")"
                   )

        VClosure _ ->
            "<closure>"

        VIo val ->
            "<IO: {VAL}>"
                |> String.replace "{VAL}" (toShowString val)


toInspectString : Value -> String
toInspectString value =
    case value of
        VInt int ->
            String.fromInt int

        VFloat float ->
            String.fromFloat float

        VString str ->
            -- TODO escaping?
            ("\""
                ++ (if String.length str > 5 then
                        String.left 5 str ++ "..."

                    else
                        str
                   )
                ++ "\""
            )
                |> String.replace "\n" "\\n"

        VChar str ->
            -- TODO escaping?
            ("'" ++ str ++ "'")
                |> String.replace "\n" "\\n"

        VBool bool ->
            if bool then
                "True"

            else
                "False"

        VUnit ->
            "()"

        VIntrinsic intrinsic ->
            "<intrinsic {ID}>"
                |> String.replace "{ID}" (Id.toString (Intrinsic.id intrinsic))

        VList values ->
            "[" ++ String.join "," (List.map toInspectString values) ++ "]"

        VTuple values ->
            "(" ++ String.join "," (List.map toInspectString values) ++ ")"

        VRecord fields ->
            "{"
                ++ (fields
                        |> Dict.toList
                        |> List.map (\( field, value_ ) -> field ++ ":" ++ toInspectString value_)
                        |> String.join ","
                   )
                ++ "}"

        VRecordGetter field ->
            "<record getter .{FIELD}>"
                |> String.replace "{FIELD}" field

        VConstructor { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "(" ++ String.join "," (List.map toInspectString args) ++ ")"
                   )

        VClosure _ ->
            "<closure>"

        VIo val ->
            "<IO: {VAL}>"
                |> String.replace "{VAL}" (toInspectString val)


isEqualityAllowed : Value -> Bool
isEqualityAllowed val =
    case val of
        VInt _ ->
            True

        VFloat _ ->
            True

        VString _ ->
            True

        VChar _ ->
            True

        VBool _ ->
            True

        VUnit ->
            True

        VRecordGetter _ ->
            True

        VList xs ->
            List.all isEqualityAllowed xs

        VTuple xs ->
            List.all isEqualityAllowed xs

        VRecord fields ->
            List.all isEqualityAllowed (Dict.values fields)

        VConstructor { args } ->
            List.all isEqualityAllowed args

        VIntrinsic _ ->
            False

        VClosure _ ->
            False

        VIo _ ->
            False


closureToString : Value -> String
closureToString val =
    case val of
        VClosure { args, body, env } ->
            AST.lambdaToString { args = args, body = body }

        _ ->
            "NOT A CLOSURE"


nothing : Value
nothing =
    VConstructor
        { id = Id.global [ "Maybe" ] "Nothing"
        , args = []
        }


just : Value -> Value
just val =
    VConstructor
        { id = Id.global [ "Maybe" ] "Just"
        , args = [ val ]
        }
