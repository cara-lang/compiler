module Operator exposing
    ( BinaryOp(..)
    , Operator(..)
    , UnaryOp(..)
    , binaryOpToString
    , toString
    , unaryOpToString
    )


type Operator
    = Binary BinaryOp
    | Unary UnaryOp


type BinaryOp
    = -- arithmetic
      Plus -- +
    | Minus -- -
    | Times -- *
    | Div -- /
    | Modulo -- %
    | Power -- **
      -- binary
    | OrBin -- |
    | AndBin -- &
    | XorBin -- ^
    | ShiftL -- <<
    | ShiftR -- >>
    | ShiftRU -- >>>, unsigned
      -- comparisons and booleans
    | Lte -- <=
    | Lt -- <
    | Eq -- ==
    | Neq -- !=
    | Gt -- >
    | Gte -- >=
    | OrBool -- ||
    | AndBool -- &&
      -- appendables
    | Append -- ++
      -- ranges
    | RangeInclusive -- ..
    | RangeExclusive -- ...


type UnaryOp
    = NegateNum -- -e
    | NegateBool -- !e
    | NegateBin -- ~e
    | InfiniteRange -- e..


toString : Operator -> String
toString op =
    case op of
        Binary bop ->
            binaryOpToString bop

        Unary uop ->
            unaryOpToString uop


binaryOpToString : BinaryOp -> String
binaryOpToString op =
    case op of
        Plus ->
            "+"

        Minus ->
            "-"

        Times ->
            "*"

        Div ->
            "/"

        Modulo ->
            "%"

        Power ->
            "**"

        OrBin ->
            "|"

        AndBin ->
            "&"

        XorBin ->
            "^"

        ShiftL ->
            "<<"

        ShiftR ->
            ">>"

        ShiftRU ->
            ">>>"

        Lte ->
            "<="

        Lt ->
            "<"

        Eq ->
            "=="

        Neq ->
            "!="

        Gt ->
            ">"

        Gte ->
            ">="

        OrBool ->
            "||"

        AndBool ->
            "&&"

        Append ->
            "++"

        RangeInclusive ->
            ".."

        RangeExclusive ->
            "..."


unaryOpToString : UnaryOp -> String
unaryOpToString op =
    case op of
        NegateNum ->
            "-"

        NegateBool ->
            "!"

        NegateBin ->
            "~"

        InfiniteRange ->
            ".."
