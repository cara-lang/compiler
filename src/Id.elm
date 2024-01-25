module Id exposing
    ( Id
    , simple, global
    , toString
    , bind, pure
    , main_
    , io, root
    )

{-|

@docs Id
@docs simple, global
@docs toString


## Important identifiers

@docs bind, pure
@docs main_


## Important modules

@docs io, root

-}


type alias Id =
    { qualifiers : List String
    , name : String
    }


toString : Id -> String
toString id =
    String.join "." (id.qualifiers ++ [ id.name ])


simple : String -> Id
simple name =
    { qualifiers = []
    , name = name
    }


global : List String -> String -> Id
global qualifiers name =
    { qualifiers = qualifiers
    , name = name
    }


bind : List String -> Id
bind monadModule =
    global monadModule "bind"


pure : List String -> Id
pure monadModule =
    global monadModule "pure"


main_ : Id
main_ =
    global [] "main"


io : String
io =
    "IO"


root : String
root =
    "<root>"
