module Intrinsic exposing
    ( Intrinsic(..)
    , all
    , id
    )

import Id exposing (Id)


type Intrinsic
    = IoPrintln
    | IoInspect
    | IoToInspectString
    | IoToString
    | IoPure
    | IoBind
    | IoReadFile
    | IoWriteFile


all : List Intrinsic
all =
    [ IoPrintln
    , IoInspect
    , IoToInspectString
    , IoToString
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

        IoInspect ->
            ioInspect

        IoToInspectString ->
            ioToInspectString

        IoToString ->
            ioToString

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


ioInspect : Id
ioInspect =
    { qualifiers = [ "IO" ]
    , name = "inspect"
    }


ioToInspectString : Id
ioToInspectString =
    { qualifiers = [ "IO" ]
    , name = "toInspectString"
    }


ioToString : Id
ioToString =
    { qualifiers = [ "IO" ]
    , name = "toString"
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
