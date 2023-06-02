port module Effect exposing
    ( Effect0(..), EffectStr(..)
    , handleEffect0, handleEffectStr
    , chdir, print, println, eprintln, writeFile, readFile
    , completedChdir, completedPrint, completedPrintln, completedEprintln, completedReadFile, completedWriteFile
    )

{-|

@docs Effect0, EffectStr
@docs handleEffect0, handleEffectStr
@docs chdir, print, println, eprintln, writeFile, readFile
@docs completedChdir, completedPrint, completedPrintln, completedEprintln, completedReadFile, completedWriteFile

-}


type Effect0
    = Print String
    | Println String
    | Eprintln String
    | WriteFile { filename : String, content : String }
    | Chdir String


type EffectStr
    = ReadFile { filename : String }


port chdir : String -> Cmd msg


port print : String -> Cmd msg


port println : String -> Cmd msg


port eprintln : String -> Cmd msg


port readFile : { filename : String } -> Cmd msg


port writeFile : { filename : String, content : String } -> Cmd msg


port completedChdir : (() -> msg) -> Sub msg


port completedPrint : (() -> msg) -> Sub msg


port completedPrintln : (() -> msg) -> Sub msg


port completedEprintln : (() -> msg) -> Sub msg


port completedReadFile : (String -> msg) -> Sub msg


port completedWriteFile : (() -> msg) -> Sub msg


handleEffect0 : Effect0 -> Cmd msg
handleEffect0 effect =
    case effect of
        Chdir path ->
            chdir path

        Print string ->
            print string

        Println string ->
            println string

        Eprintln string ->
            eprintln string

        WriteFile r ->
            writeFile r


handleEffectStr : EffectStr -> Cmd msg
handleEffectStr effect =
    case effect of
        ReadFile r ->
            readFile r
