module Tron.Focus exposing (..)


type alias Level = Int


type Focused
    = FocusedBy Level
    | NotFocused


type Direction
    = Up
    | Down
    | Right
    | Left


toString : Focused -> String
toString focus =
    case focus of
        FocusedBy level -> "focus--" ++ String.fromInt level
        NotFocused -> ""
