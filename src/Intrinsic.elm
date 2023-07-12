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
    | IoReadFile
    | IoWriteFile


all : List Intrinsic
all =
    [ IoPrintln
    , IoPure
    , IoBind
    , IoReadFile
    , IoWriteFile
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

        IoReadFile ->
            ioReadFile

        IoWriteFile ->
            ioWriteFile


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


ioReadFile : Id
ioReadFile =
    { qualifiers = [ "IO" ]
    , name = "readFile"
    }


ioWriteFile : Id
ioWriteFile =
    { qualifiers = [ "IO" ]
    , name = "writeFile"
    }
