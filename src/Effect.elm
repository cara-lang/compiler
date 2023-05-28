module Effect exposing (Effect(..))


type Effect
    = Println String
    | Eprintln String
    | ReadFile { filename : String }
    | WriteFile { filename : String, content : String }
