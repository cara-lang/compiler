module Operator exposing (Operator(..), lexeme)


type Operator
    = InclusiveRange
    | Minus
    | Negate
    | LogicalAnd
    | LogicalOr
    | Append
    | ExclusiveRange
    | BinaryOr
    | BinaryXor
    | BinaryAnd
    | Eq
    | Neq
    | Lte
    | Lt
    | Gt
    | Gte
    | Shl
    | Shr
    | Shru
    | Plus
    | Times
    | Div
    | Modulo
    | Power


lexeme : Operator -> String
lexeme op =
    case op of
        InclusiveRange ->
            ".."

        Minus ->
            "-"

        Negate ->
            "!"

        LogicalAnd ->
            "&&"

        LogicalOr ->
            "||"

        Append ->
            "++"

        ExclusiveRange ->
            "..."

        BinaryOr ->
            "|"

        BinaryXor ->
            "^"

        BinaryAnd ->
            "&"

        Eq ->
            "=="

        Neq ->
            "!="

        Lte ->
            "<="

        Lt ->
            "<"

        Gt ->
            ">"

        Gte ->
            ">="

        Shl ->
            "<<"

        Shr ->
            ">>"

        Shru ->
            ">>>"

        Plus ->
            "+"

        Times ->
            "*"

        Div ->
            "/"

        Modulo ->
            "%"

        Power ->
            "**"
