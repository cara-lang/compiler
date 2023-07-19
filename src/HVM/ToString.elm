module HVM.ToString exposing (binOp, file, rule, term)

import HVM.AST exposing (BinOp(..), File, Rule, Term(..))


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


rule : Rule -> String
rule r =
    "{LHS} = {RHS}"
        |> String.replace "{LHS}" (term r.lhs)
        |> String.replace "{RHS}" (term r.rhs)


file : File -> String
file f =
    f.rules
        |> List.map rule
        |> String.join "\n"


term : Term -> String
term t =
    case t of
        Var name ->
            name

        Dup { leftName, rightName, expr, body } ->
            "dup {LEFT} {RIGHT} = {EXPR}; {BODY}"
                |> String.replace "{LEFT}" leftName
                |> String.replace "{RIGHT}" rightName
                |> String.replace "{EXPR}" (term expr)
                |> String.replace "{BODY}" (term body)

        Sup { left, right } ->
            "{{LEFT} {RIGHT}}"
                |> String.replace "{LEFT}" (term left)
                |> String.replace "{RIGHT}" (term right)

        Let { name, expr, body } ->
            "let {NAME} = {EXPR}; {BODY}"
                |> String.replace "{NAME}" name
                |> String.replace "{EXPR}" (term expr)
                |> String.replace "{BODY}" (term body)

        Lam { name, body } ->
            "@{NAME} {BODY}"
                |> String.replace "{NAME}" name
                |> String.replace "{BODY}" (term body)

        App { function, arg } ->
            "({FUNCTION} {ARG})"
                |> String.replace "{FUNCTION}" (term function)
                |> String.replace "{ARG}" (term arg)

        Ctr { name, args } ->
            if List.isEmpty args then
                "({NAME})"
                    |> String.replace "{NAME}" name

            else
                "({NAME} {ARGS})"
                    |> String.replace "{NAME}" name
                    |> String.replace "{ARGS}"
                        (args
                            |> List.map term
                            |> String.join " "
                        )

        U60 n ->
            String.fromInt n

        F60 n ->
            String.fromFloat n

        Op2 { op, left, right } ->
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
