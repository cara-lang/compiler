module Tree.Zipper.Extra exposing
    ( breadcrumbs
    , findChild
    , isLast
    , navigate
    )

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


navigate : (String -> a -> Bool) -> List String -> Zipper a -> Maybe (Zipper a)
navigate hasName names zipper =
    case names of
        [] ->
            Just zipper

        name :: rest ->
            case findChild (hasName name) zipper of
                Nothing ->
                    Nothing

                Just inChild ->
                    navigate hasName rest inChild


isLast : Zipper a -> Bool
isLast zipper =
    Zipper.forward zipper == Nothing


breadcrumbs : Zipper a -> List a
breadcrumbs zipper =
    let
        go : List a -> Maybe (Zipper a) -> List a
        go acc parent =
            case parent of
                Nothing ->
                    acc

                Just parent_ ->
                    go (Zipper.label parent_ :: acc) (Zipper.parent parent_)
    in
    go [ Zipper.label zipper ] (Zipper.parent zipper)
