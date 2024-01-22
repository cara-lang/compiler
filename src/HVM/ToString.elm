module HVM.ToString exposing
    ( adt
    , binOp
    , file
    , pattern
    , rule
    , term
    )

import HVM.AST
    exposing
        ( ADT
        , ADTConstructor
        , BinOp(..)
        , File
        , Pattern(..)
        , Rule
        , Term(..)
        )


binOp : BinOp -> String
binOp op =
    case op of
        Add ->
            "+"

        Sub ->
            "-"

        Mul ->
            "*"

        Div ->
            "/"

        Mod ->
            "%"

        And ->
            "&"

        Or ->
            "|"

        Xor ->
            "^"

        Shl ->
            "<<"

        Shr ->
            ">>"

        Lte ->
            "<="

        Ltn ->
            "<"

        Eql ->
            "=="

        Gte ->
            ">="

        Gtn ->
            ">"

        Neq ->
            "!="

        Not ->
            "~"


rule : Rule -> String
rule r =
    if List.isEmpty r.args then
        "{NAME} = {BODY}"
            |> String.replace "{NAME}" r.functionName
            |> String.replace "{BODY}" (term r.body)

    else
        "({NAME} {ARGS}) = {BODY}"
            |> String.replace "{NAME}" r.functionName
            |> String.replace "{ARGS}"
                (String.join " "
                    (List.map pattern r.args)
                )
            |> String.replace "{BODY}" (term r.body)


adt : ADT -> String
adt t =
    "data {NAME} = {CTRS}"
        |> String.replace "{NAME}" t.name
        |> String.replace "{CTRS}"
            (String.join " | "
                (List.map adtConstructor t.constructors)
            )


adtConstructor : ADTConstructor -> String
adtConstructor ctr =
    if ctr.arity == 0 then
        ctr.name

    else
        "({NAME} {ARGS})"
            |> String.replace "{NAME}" ctr.name
            |> String.replace "{ARGS}"
                (String.join " "
                    (List.range 1 ctr.arity
                        |> List.map (\i -> "a" ++ String.fromInt i)
                    )
                )


pattern : Pattern -> String
pattern p =
    case p of
        PWildcard ->
            "*"

        PVar var ->
            var

        PCtr name args ->
            "({NAME} {BODY})"
                |> String.replace "{NAME}" name
                |> String.replace "{BODY}"
                    (String.join " "
                        (List.map pattern args)
                    )

        PTup ( first, second ) ->
            "({FIRST},{SECOND})"
                |> String.replace "{FIRST}" (pattern first)
                |> String.replace "{SECOND}" (pattern second)

        PList patterns ->
            "[{LIST}]"
                |> String.replace "{LIST}"
                    (String.join ","
                        (List.map pattern patterns)
                    )


file : File -> String
file f =
    [ List.map adt f.adts
    , List.map rule f.rules
    ]
        |> List.concat
        |> String.join "\n"


term : Term -> String
term t =
    case t of
        Var name ->
            name

        Let { pat, value, next } ->
            "let {PAT} = {VAL}; {NEXT}"
                |> String.replace "{PAT}" (pattern pat)
                |> String.replace "{VAL}" (term value)
                |> String.replace "{NEXT}" (term next)

        Lam { name, body } ->
            "(@{NAME} {BODY})"
                |> String.replace "{NAME}" name
                |> String.replace "{BODY}" (term body)

        App { function, args } ->
            "({FUNCTION} {ARGS})"
                |> String.replace "{FUNCTION}" (term function)
                |> String.replace "{ARGS}"
                    (String.join " "
                        (List.map term args)
                    )

        U60 n ->
            String.fromInt n

        Opx { op, left, right } ->
            "({OP} {LEFT} {RIGHT})"
                |> String.replace "{OP}" (binOp op)
                |> String.replace "{LEFT}" (term left)
                |> String.replace "{RIGHT}" (term right)

        Str string ->
            "\"{STRING}\""
                |> String.replace "{STRING}"
                    (string
                        |> String.replace "\"" "'"
                    )

        Lst list ->
            "[{ITEMS}]"
                |> String.replace "{ITEMS}"
                    (list
                        |> List.map term
                        |> String.join ", "
                    )

        Tup ( first, second ) ->
            "({FIRST},{SECOND})"
                |> String.replace "{FIRST}" (term first)
                |> String.replace "{SECOND}" (term second)

        Match { value, arms } ->
            "match {VALUE} {\n{ARMS}\n}"
                |> String.replace "{VALUE}" (term value)
                |> String.replace "{ARMS}"
                    (String.join "\n"
                        (List.map
                            (\( pat, body ) ->
                                "    {PAT}: {BODY}"
                                    |> String.replace "{PAT}" (pattern pat)
                                    |> String.replace "{BODY}" (term body)
                            )
                            arms
                        )
                    )

        Era ->
            "*"
