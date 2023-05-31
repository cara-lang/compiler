module Id exposing (Id, isIoPrintln, toString)


type alias Id =
    { qualifiers : List String
    , name : String
    }


toString : Id -> String
toString id =
    String.join "." (id.qualifiers ++ [ id.name ])


isIoPrintln : Id -> Bool
isIoPrintln id =
    id == ioPrintln


ioPrintln : Id
ioPrintln =
    { qualifiers = [ "IO" ]
    , name = "println"
    }
