module Id.Qualified exposing
    ( QualifiedId, id
    , toId, toString
    )

{-|

@docs QualifiedId, id
@docs toId, toString

-}

import Id exposing (Id)
import NonemptyList exposing (NonemptyList)


type alias QualifiedId =
    { qualifiers : NonemptyList String
    , name : String
    }


id : NonemptyList String -> String -> QualifiedId
id qualifiers name =
    { qualifiers = qualifiers
    , name = name
    }


toId : QualifiedId -> Id
toId id_ =
    { qualifiers = NonemptyList.toList id_.qualifiers
    , name = id_.name
    }


toString : QualifiedId -> String
toString id_ =
    id_
        |> toId
        |> Id.toString
