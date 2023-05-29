module AST exposing
    ( AST(..)
    , Id
    , idToString
    , toString
    )

import String.Extra as String
import Tree
import Tree.Zipper as Zipper exposing (Zipper)


type alias Id =
    { qualifiers : List String
    , name : String
    }


type AST
    = Program { filename : String }
    | Let { name : String }
    | Int Int
    | Module { name : String }
    | Println
    | Var Id
    | RootVar Id


idToString : Id -> String
idToString id =
    String.join "." (id.qualifiers ++ [ id.name ])


toString : Zipper AST -> String
toString ast =
    ast
        |> Zipper.toTree
        |> Tree.restructure
            astToString
            (\self children ->
                """{SELF}
{CHILDREN}"""
                    |> String.replace "{SELF}" self
                    |> String.replace "{CHILDREN}"
                        (String.join "\n" children
                            |> String.trimRight
                            |> String.indent
                        )
            )


astToString : AST -> String
astToString ast =
    case ast of
        Program { filename } ->
            "Program {FILENAME}"
                |> String.replace "{FILENAME}" filename

        Let { name } ->
            "Let {NAME}"
                |> String.replace "{NAME}" name

        Int n ->
            "Int {N}"
                |> String.replace "{N}" (String.fromInt n)

        Module m ->
            "Module {NAME}"
                |> String.replace "{NAME}" m.name

        Println ->
            "Println"

        Var id ->
            "Var {ID}"
                |> String.replace "{ID}" (idToString id)

        RootVar id ->
            "RootVar {ID}"
                |> String.replace "{ID}" (idToString id)
