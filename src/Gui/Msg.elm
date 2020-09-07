module Gui.Msg exposing (..)


import Gui.Property exposing (Path)
import Gui.Mouse exposing (MouseAction)


type Msg
    = NoOp
    | ApplyMouse MouseAction -- from the document
    | Click Path
    | MouseDown Path
    | KeyDown Int
