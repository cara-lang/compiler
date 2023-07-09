module Id exposing
    ( Id
    , local, global
    , toString
    , bind, pure
    , io
    )

{-|

@docs Id
@docs local, global
@docs toString
@docs bind, pure
@docs io

-}


type alias Id =
    { qualifiers : List String
    , name : String
    }


toString : Id -> String
toString id =
    String.join "." (id.qualifiers ++ [ id.name ])


local : String -> Id
local name =
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


io : List String
io =
    [ "IO" ]
