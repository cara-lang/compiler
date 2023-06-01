module Env exposing
    ( Env, initWithIntrinsics
    , add, addId, addDict, createModule
    , open, goUp
    , get, toString
    , Module
    )

{-|

@docs Env, initWithIntrinsics
@docs add, addId, addDict, createModule
@docs open, goUp
@docs get, toString
@docs Module

-}

import Dict exposing (Dict)
import EnvDict exposing (EnvDict)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)
import String.Extra as String
import Tree
import Tree.Zipper as Zipper exposing (Zipper)
import Tree.Zipper.Extra as Zipper
import Value exposing (Value(..))


type alias Env =
    Zipper Module


type alias Module =
    { name : String

    -- TODO private/public
    , values : Dict String Value
    }


initWithIntrinsics : Env
initWithIntrinsics =
    rootModule
        |> Tree.singleton
        |> Zipper.fromTree
        |> addIntrinsics


rootModule : Module
rootModule =
    emptyModule "<root>"


emptyModule : String -> Module
emptyModule name =
    { name = name
    , values = Dict.empty
    }


addIntrinsics : Env -> Env
addIntrinsics env =
    let
        addIntrinsic : Intrinsic -> Env -> Env
        addIntrinsic intrinsic module_ =
            let
                id =
                    Intrinsic.id intrinsic
            in
            env
                |> Zipper.mapRoot .name (addId id (VIntrinsic intrinsic))
    in
    Intrinsic.all
        |> List.foldl addIntrinsic env


add : String -> Value -> Env -> Env
add name value env =
    env
        |> Zipper.mapLabel (\m -> { m | values = Dict.insert name value m.values })


addId : Id -> Value -> Env -> Env
addId id value env =
    env
        |> ensurePath id.qualifiers
        |> Zipper.mapAtPath
            .name
            id.qualifiers
            (add id.name value)


addDict : EnvDict -> Env -> Env
addDict dict env =
    dict
        |> EnvDict.toList
        |> List.foldl (\( id, value ) env_ -> addId id value env_) env


createModule : String -> Env -> Env
createModule name env =
    if List.any (\m -> (Tree.label m).name == name) (Zipper.children env) then
        env

    else
        env
            |> Zipper.mapTree (Tree.appendChild (Tree.singleton (emptyModule name)))


ensurePath : List String -> Env -> Env
ensurePath path env =
    let
        goUpNTimes : Int -> Env -> Env
        goUpNTimes n env_ =
            if n <= 0 then
                env_

            else
                case Zipper.parent env_ of
                    Nothing ->
                        Debug.todo "Bug: couldn't go up the specified number of times"

                    Just env__ ->
                        goUpNTimes (n - 1) env__

        go : List String -> Env -> Env
        go path_ env_ =
            case path_ of
                [] ->
                    env_
                        |> goUpNTimes (List.length path)

                m :: rest ->
                    case
                        env_
                            |> createModule m
                            |> open [ m ]
                    of
                        Nothing ->
                            Debug.todo "Bug: couldn't open a freshly created module"

                        Just newEnv ->
                            go rest newEnv
    in
    go path env


open : List String -> Env -> Maybe Env
open modules env =
    Zipper.navigate .name modules env


get : Id -> Env -> Maybe Value
get id env =
    Zipper.navigate .name id.qualifiers env
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


goUp : Env -> Maybe Env
goUp env =
    Zipper.parent env
