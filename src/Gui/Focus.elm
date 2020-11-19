module Gui.Focus exposing (..)


import Gui.Path as Path exposing (Path)

import Array
import Array exposing (Array)


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
