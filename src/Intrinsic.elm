module Intrinsic exposing
    ( Intrinsic(..)
    , all
    , id
    )

import Id exposing (Id)


type Intrinsic
    = IoPrintln
    | IoPure
    | IoBind


all : List Intrinsic
all =
    [ IoPrintln
    , IoPure
    , IoBind
    ]


id : Intrinsic -> Id
id intrinsic =
    case intrinsic of
        IoPrintln ->
            ioPrintln

        IoPure ->
            ioPure

        IoBind ->
            ioBind


ioPrintln : Id
ioPrintln =
    { qualifiers = [ "IO" ]
    , name = "println"
    }


ioPure : Id
ioPure =
    { qualifiers = [ "IO" ]
    , name = "pure"
    }


ioBind : Id
ioBind =
    { qualifiers = [ "IO" ]
    , name = "bind"
    }
