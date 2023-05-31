module Effect exposing
    ( Effect0(..)
    , EffectStr(..)
    )


type Effect0
    = Println String
    | Eprintln String
    | WriteFile { filename : String, content : String }


type EffectStr
    = ReadFile { filename : String }
