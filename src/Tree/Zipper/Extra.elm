module Tree.Zipper.Extra exposing
    ( appendChild
    , breadcrumbs
    , findChild
    , isLast
    , isRoot
    , mapAtPath
    , mapRoot
    , navigate
    )

import Tree exposing (Tree)
import Tree.Zipper as Zipper exposing (Zipper)


findChild : (a -> Bool) -> Zipper a -> Maybe (Zipper a)
findChild pred zipper =
    let
        go : Maybe (Zipper a) -> Maybe (Zipper a)
        go maybeChild =
            case maybeChild of
                Nothing ->
                    Nothing

                Just child ->
                    if pred (Zipper.label child) then
                        Just child

                    else
                        go (Zipper.nextSibling child)
    in
    go (Zipper.firstChild zipper)


navigate : (a -> comparable) -> List comparable -> Zipper a -> Maybe (Zipper a)
navigate getId ids zipper =
    case ids of
        [] ->
            Just zipper

        id :: rest ->
            case findChild (\a -> getId a == id) zipper of
                Nothing ->
                    Nothing

                Just inChild ->
                    navigate getId rest inChild


isLast : Zipper a -> Bool
isLast zipper =
    Zipper.forward zipper == Nothing


isRoot : Zipper a -> Bool
isRoot zipper =
    Zipper.parent zipper == Nothing


{-| ["grandparent", "parent", "current"]

Doesn't contain the current module name.
Doesn't contain the root.

-}
breadcrumbs : Zipper a -> List a
breadcrumbs zipper =
    let
        go : List a -> Zipper a -> List a
        go acc current =
            case Zipper.parent current of
                Nothing ->
                    -- We're the root. We don't want the "<root>" root label in breadcrumbs so let's stop here!
                    acc

                Just parent ->
                    go (Zipper.label current :: acc) parent
    in
    go [] zipper


{-| Appends a child (to the end of children), or makes a singleton tree into
a tree with a single child.

Keeps focus on the parent.

-}
appendChild : Tree a -> Zipper a -> Zipper a
appendChild child zipper =
    case Zipper.lastChild zipper of
        Nothing ->
            zipper
                |> Zipper.mapTree
                    (\t ->
                        Tree.tree
                            (Tree.label t)
                            [ child ]
                    )

        Just lastChild ->
            case
                lastChild
                    |> Zipper.append child
                    |> Zipper.parent
            of
                Nothing ->
                    Debug.todo "Couldn't go to a parent after appending a child"

                Just parent ->
                    parent


mapRoot : (a -> comparable) -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
mapRoot getId fn zipper =
    let
        path : List comparable
        path =
            breadcrumbs zipper
                |> List.map getId
    in
    case
        zipper
            |> Zipper.root
            |> fn
            |> navigate getId path
    of
        Nothing ->
            Debug.todo "mapRoot: Bug: couldn't retrace steps from root to previous focus"

        Just zipper_ ->
            zipper_


mapAtPath : (a -> comparable) -> List comparable -> (Zipper a -> Zipper a) -> Zipper a -> Zipper a
mapAtPath getId path fn zipper =
    case
        zipper
            |> navigate getId path
            |> Maybe.andThen
                (\zipperAtPath ->
                    let
                        originalPath : List comparable
                        originalPath =
                            breadcrumbs zipper
                                |> List.map getId
                    in
                    zipperAtPath
                        |> fn
                        |> Zipper.root
                        |> navigate getId originalPath
                )
    of
        Nothing ->
            Debug.todo "mapAtPath: Bug: couldn't retrace steps to original focus"

        Just zipper_ ->
            zipper_
