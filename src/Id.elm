module Id exposing (Id, local, toString)


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
