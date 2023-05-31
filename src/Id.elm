module Id exposing (Id, toString)


type alias Id =
    { qualifiers : List String
    , name : String
    }


toString : Id -> String
toString id =
    String.join "." (id.qualifiers ++ [ id.name ])
