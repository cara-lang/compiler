module Value exposing (Value(..), toString)

import AST exposing (Expr, Pattern)
import Env exposing (Env)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)


type Value
    = VInt Int
    | VFloat Float
    | VString String
    | VBool Bool
    | VUnit
    | VIntrinsic Intrinsic
    | VList (List Value)
    | VTuple (List Value)
    | VRecordGetter String
    | VConstructor { id : Id, args : List Value }
    | VClosure { args : List Pattern, body : Expr, env : Env Value }


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
            "[" ++ String.join "," (List.map toString values) ++ "]"

        VTuple values ->
            "(" ++ String.join "," (List.map toString values) ++ ")"

        VRecordGetter field ->
            "<record getter .{FIELD}>"
                |> String.replace "{FIELD}" field

        VConstructor { id, args } ->
            Id.toString id
                ++ (if List.isEmpty args then
                        ""

                    else
                        "(" ++ String.join "," (List.map toString args) ++ ")"
                   )

        VClosure { args, body, env } ->
            Debug.todo "toString VClosure"
