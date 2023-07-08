module Env exposing
    ( Env, initWithIntrinsics
    , add, addId, addDict, addBinaryOp
    , createModule
    , open, goUp
    , get, getBinaryOp, toString
    , Module
    )

{-|

@docs Env, initWithIntrinsics
@docs add, addId, addDict, addBinaryOp
@docs createModule
@docs open, goUp
@docs get, getBinaryOp, toString
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


type alias Env value =
    Zipper (Module value)


type alias Module value =
    { name : String

    -- TODO private/public
    , values : Dict String value
    , binaryOps : Dict String (List value)
    }


initWithIntrinsics : { intrinsicToValue : Intrinsic -> value } -> Env value
initWithIntrinsics cfg =
    rootModule
        |> Tree.singleton
        |> Zipper.fromTree
        |> addIntrinsics cfg


rootModule : Module value
rootModule =
    emptyModule "<root>"


emptyModule : String -> Module value
emptyModule name =
    { name = name
    , values = Dict.empty
    , binaryOps = Dict.empty
    }


addIntrinsics : { intrinsicToValue : Intrinsic -> value } -> Env value -> Env value
addIntrinsics { intrinsicToValue } env =
    let
        addIntrinsic : Intrinsic -> Env value -> Env value
        addIntrinsic intrinsic module_ =
            let
                id =
                    Intrinsic.id intrinsic
            in
            env
                |> Zipper.mapRoot .name (addId id (intrinsicToValue intrinsic))
    in
    Intrinsic.all
        |> List.foldl addIntrinsic env


add : String -> value -> Env value -> Env value
add name value env =
    env
        |> Zipper.mapLabel (\m -> { m | values = Dict.insert name value m.values })


addId : Id -> value -> Env value -> Env value
addId id value env =
    env
        |> ensurePath id.qualifiers
        |> Zipper.mapAtPath
            .name
            id.qualifiers
            (add id.name value)


addDict : EnvDict value -> Env value -> Env value
addDict dict env =
    dict
        |> EnvDict.toList
        |> List.foldl (\( id, value ) env_ -> addId id value env_) env


addBinaryOp : String -> value -> Env value -> Env value
addBinaryOp op impl env =
    env
        |> Zipper.mapLabel
            (\m ->
                { m
                    | binaryOps =
                        Dict.update
                            op
                            (Maybe.withDefault []
                                >> (::) impl
                                >> Just
                            )
                            m.binaryOps
                }
            )


createModule : String -> Env value -> Env value
createModule name env =
    if List.any (\m -> (Tree.label m).name == name) (Zipper.children env) then
        env

    else
        env
            |> Zipper.mapTree (Tree.appendChild (Tree.singleton (emptyModule name)))


ensurePath : List String -> Env value -> Env value
ensurePath path env =
    let
        goUpNTimes : Int -> Env value -> Env value
        goUpNTimes n env_ =
            if n <= 0 then
                env_

            else
                case Zipper.parent env_ of
                    Nothing ->
                        Debug.todo "Bug: couldn't go up the specified number of times"

                    Just env__ ->
                        goUpNTimes (n - 1) env__

        go : List String -> Env value -> Env value
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


open : List String -> Env value -> Maybe (Env value)
open modules env =
    Zipper.navigate .name modules env


get : Id -> Env value -> Maybe value
get id env =
    Zipper.navigate .name id.qualifiers env
        |> Maybe.andThen
            (\deepEnv ->
                (Zipper.label deepEnv).values
                    |> Dict.get id.name
            )


getBinaryOp : String -> Env value -> Maybe (List value)
getBinaryOp binop env =
    Dict.get binop (Zipper.label env).binaryOps


toString : { valueToString : value -> String } -> Env value -> String
toString cfg env =
    env
        |> Zipper.toTree
        |> Tree.restructure
            (moduleToString cfg)
            (\self children ->
                """{SELF}
{CHILDREN}"""
                    |> String.replace "{SELF}" self
                    |> String.replace "{CHILDREN}"
                        (String.join "\n" children
                            |> String.indent
                        )
            )


moduleToString : { valueToString : value -> String } -> Module value -> String
moduleToString { valueToString } module_ =
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
                            |> String.replace "{VALUE}" (valueToString v)
                    )
                |> String.join "\n"
            )


goUp : Env value -> Maybe (Env value)
goUp env =
    Zipper.parent env
