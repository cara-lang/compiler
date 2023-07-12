port module Effect exposing
    ( Effect0(..), EffectStr(..), EffectMaybeStr(..), EffectBool(..)
    , handleEffect0, handleEffectStr, handleEffectMaybeStr, handleEffectBool
    , chdir, print, println, eprintln, writeFile, writeFileMaybe, readFile, readFileMaybe
    , completedChdir, completedPrint, completedPrintln, completedEprintln, completedWriteFile, completedWriteFileMaybe, completedReadFile, completedReadFileMaybe
    )

{-|

@docs Effect0, EffectStr, EffectMaybeStr, EffectBool
@docs handleEffect0, handleEffectStr, handleEffectMaybeStr, handleEffectBool
@docs chdir, print, println, eprintln, writeFile, writeFileMaybe, readFile, readFileMaybe
@docs completedChdir, completedPrint, completedPrintln, completedEprintln, completedWriteFile, completedWriteFileMaybe, completedReadFile, completedReadFileMaybe

-}


type Effect0
    = Print String
    | Println String
    | Eprintln String
    | WriteFile { filename : String, content : String }
    | Chdir String


type EffectStr
    = ReadFile { filename : String }


type EffectMaybeStr
    = ReadFileMaybe { filename : String }


type EffectBool
    = WriteFileMaybe { filename : String, content : String }


port chdir : String -> Cmd msg


port print : String -> Cmd msg


port println : String -> Cmd msg


port eprintln : String -> Cmd msg


port readFile : { filename : String } -> Cmd msg


port readFileMaybe : { filename : String } -> Cmd msg


port writeFile : { filename : String, content : String } -> Cmd msg


port writeFileMaybe : { filename : String, content : String } -> Cmd msg


port completedChdir : (() -> msg) -> Sub msg


port completedPrint : (() -> msg) -> Sub msg


port completedPrintln : (() -> msg) -> Sub msg


port completedEprintln : (() -> msg) -> Sub msg


port completedReadFile : (String -> msg) -> Sub msg


port completedReadFileMaybe : (Maybe String -> msg) -> Sub msg


port completedWriteFile : (() -> msg) -> Sub msg


port completedWriteFileMaybe : (Bool -> msg) -> Sub msg


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


handleEffectMaybeStr : EffectMaybeStr -> Cmd msg
handleEffectMaybeStr effect =
    case effect of
        ReadFileMaybe r ->
            readFileMaybe r


handleEffectBool : EffectBool -> Cmd msg
handleEffectBool effect =
    case effect of
        WriteFileMaybe r ->
            writeFileMaybe r
