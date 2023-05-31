module Intrinsic exposing
    ( Intrinsic(..)
    , all
    , id
    , is
    )

import Id exposing (Id)


type Intrinsic
    = IoPrintln


all : List Intrinsic
all =
    [ IoPrintln
    ]


id : Intrinsic -> Id
id intrinsic =
    case intrinsic of
        IoPrintln ->
            ioPrintln


is : Intrinsic -> Id -> Bool
is intrinsic id_ =
    id_ == id intrinsic


ioPrintln : Id
ioPrintln =
    { qualifiers = [ "IO" ]
    , name = "println"
    }
