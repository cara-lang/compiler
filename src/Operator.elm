module Operator exposing (Operator(..), id)

import Id exposing (Id)


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


id : Operator -> Id
id op =
    case op of
        InclusiveRange ->
            Id.local ".."

        Minus ->
            Id.local "-"

        Negate ->
            Id.local "!"

        LogicalAnd ->
            Id.local "&&"

        LogicalOr ->
            Id.local "||"

        Append ->
            Id.local "++"

        ExclusiveRange ->
            Id.local "..."

        BinaryOr ->
            Id.local "|"

        BinaryXor ->
            Id.local "^"

        BinaryAnd ->
            Id.local "&"

        Eq ->
            Id.local "=="

        Neq ->
            Id.local "!="

        Lte ->
            Id.local "<="

        Lt ->
            Id.local "<"

        Gt ->
            Id.local ">"

        Gte ->
            Id.local ">="

        Shl ->
            Id.local "<<"

        Shr ->
            Id.local ">>"

        Shru ->
            Id.local ">>>"

        Plus ->
            Id.local "+"

        Times ->
            Id.local "*"

        Div ->
            Id.local "/"

        Modulo ->
            Id.local "%"

        Power ->
            Id.local "**"
