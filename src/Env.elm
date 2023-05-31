module Env exposing
    ( Env, empty
    , add, addModule, open
    , get
    , toString
    , Module
    )

{-|

@docs Env, empty
@docs add, addModule, open
@docs get
@docs toString
@docs Module

-}

import Dict exposing (Dict)
import Id exposing (Id)
import String.Extra as String
import Tree
import Tree.Zipper as Zipper exposing (Zipper)
import Tree.Zipper.Extra as Zipper
import Value exposing (Value)


type alias Env =
    Zipper Module


type alias Module =
    { name : String

    -- TODO private/public
    , values : Dict String Value
    }


empty : Env
empty =
    emptyModule "<root>"
        |> Tree.singleton
        |> Zipper.fromTree


emptyModule : String -> Module
emptyModule name =
    { name = name
    , values = Dict.empty
    }


add : String -> Value -> Env -> Env
add name value env =
    env
        |> Zipper.mapLabel (\m -> { m | values = Dict.insert name value m.values })


addModule : String -> Env -> Env
addModule name env =
    if List.any (\m -> (Tree.label m).name == name) (Zipper.children env) then
        env

    else
        env
            |> Zipper.mapTree (Tree.appendChild (Tree.singleton (emptyModule name)))


open : List String -> Env -> Maybe Env
open modules env =
    Zipper.navigate (\name m -> m.name == name) modules env


get : Id -> Env -> Maybe Value
get id env =
    Zipper.navigate (\name m -> m.name == name) id.qualifiers env
        |> Maybe.andThen
            (\deepEnv ->
                (Zipper.label deepEnv).values
                    |> Dict.get id.name
            )


toString : Env -> String
toString env =
    env
        |> Zipper.toTree
        |> Tree.restructure
            moduleToString
            (\self children ->
                """{SELF}
{CHILDREN}"""
                    |> String.replace "{SELF}" self
                    |> String.replace "{CHILDREN}"
                        (String.join "\n" children
                            |> String.indent
                        )
            )


moduleToString : Module -> String
moduleToString module_ =
    """{MODULE}:
{VALUES}"""
        |> String.replace "{MODULE}" module_.name
        |> String.replace "{VALUES}"
            (module_.values
                |> Dict.toList
                |> List.map
                    (\( k, v ) ->
                        "    {KEY} => {VALUE}"
                            |> String.replace "{KEY}" k
                            |> String.replace "{VALUE}" (Value.toString v)
                    )
                |> String.join "\n"
            )
