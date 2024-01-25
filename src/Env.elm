module Env exposing
    ( Env, initWithIntrinsics
    , add, addId, addBinaryOp, addUnaryOp
    , createModule, rootModule
    , goInto, goUp
    , get, getBinaryOp, getUnaryOp
    , toString
    , localId, localize
    , Module
    )

{-| Everything in a given Env Module will be fully qualified. Thus, x => Bar.Foo(123) instead of x => Foo(123)

@docs Env, initWithIntrinsics
@docs add, addId, addBinaryOp, addUnaryOp
@docs createModule, rootModule
@docs goInto, goUp
@docs get, getBinaryOp, getUnaryOp
@docs toString
@docs localId, localize
@docs Module

-}

import Basics.Extra as Basics
import Dict exposing (Dict)
import Id exposing (Id)
import Intrinsic exposing (Intrinsic)
import String.Extra as String
import Tree
import Tree.Zipper as Zipper exposing (Zipper)
import Tree.Zipper.Extra as ZipperExtra


type alias Env value =
    Zipper (Module value)


type alias Module value =
    { name : String

    -- TODO private/public
    , values : Dict String value
    , binaryOps : Dict String (List value)
    , unaryOps : Dict String (List value)
    }


initWithIntrinsics : { intrinsicToValue : Intrinsic -> value } -> Env value
initWithIntrinsics cfg =
    rootModule
        |> Tree.singleton
        |> Zipper.fromTree
        |> addIntrinsics cfg


rootModule : Module value
rootModule =
    emptyModule Id.root


emptyModule : String -> Module value
emptyModule name =
    { name = name
    , values = Dict.empty
    , binaryOps = Dict.empty
    , unaryOps = Dict.empty
    }


addIntrinsics : { intrinsicToValue : Intrinsic -> value } -> Env value -> Env value
addIntrinsics { intrinsicToValue } env =
    let
        addIntrinsic : Intrinsic -> Env value -> Env value
        addIntrinsic intrinsic env_ =
            let
                id =
                    Intrinsic.id intrinsic
            in
            env_
                |> ZipperExtra.mapRoot .name (addId id (intrinsicToValue intrinsic))
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
        |> ZipperExtra.mapAtPath
            .name
            id.qualifiers
            (add id.name value)


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


addUnaryOp : String -> value -> Env value -> Env value
addUnaryOp op impl env =
    env
        |> Zipper.mapLabel
            (\m ->
                { m
                    | unaryOps =
                        Dict.update
                            op
                            (Maybe.withDefault []
                                >> (::) impl
                                >> Just
                            )
                            m.unaryOps
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
            Basics.doNTimes
                n
                (\e ->
                    case Zipper.parent e of
                        Nothing ->
                            Debug.todo "Bug: couldn't go up the specified number of times"

                        Just env__ ->
                            env__
                )
                env_

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
                            |> goInto [ m ]
                    of
                        Nothing ->
                            Debug.todo "Bug: couldn't go into a freshly created module"

                        Just newEnv ->
                            go rest newEnv
    in
    go path env


goInto : List String -> Env value -> Maybe (Env value)
goInto modules env =
    ZipperExtra.navigate .name modules env


get : Id -> Env value -> Maybe value
get id env =
    ZipperExtra.navigate .name id.qualifiers env
        |> Maybe.andThen
            (\deepEnv ->
                (Zipper.label deepEnv).values
                    |> Dict.get id.name
            )


getBinaryOp : String -> Env value -> Maybe (List value)
getBinaryOp binaryOp env =
    Dict.get binaryOp (Zipper.label env).binaryOps


getUnaryOp : String -> Env value -> Maybe (List value)
getUnaryOp unaryOp env =
    Dict.get unaryOp (Zipper.label env).unaryOps


toString : { valueToString : value -> String } -> Env value -> String
toString cfg env =
    env
        |> Zipper.toTree
        |> Tree.restructure
            (moduleToString cfg)
            (\self children ->
                """
{SELF}
{CHILDREN}
"""
                    |> String.replace "{SELF}" self
                    |> String.replace "{CHILDREN}"
                        (String.concat children
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


localId : Env value -> String -> Id
localId env name =
    let
        qualifiers =
            ZipperExtra.breadcrumbs env
                |> List.map .name
    in
    Id.global qualifiers name


localize : Env value -> Id -> Id
localize env id =
    let
        extraQualifiers =
            ZipperExtra.breadcrumbs env
                |> List.map .name
    in
    Id.global (extraQualifiers ++ id.qualifiers) id.name
