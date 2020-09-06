module Gui.Msg exposing (..)


import Gui.Control exposing (..)
import Gui.Mouse exposing (..)
import Gui.Focus exposing (..)


type Msg
    = NoOp
    | ApplyMouse MouseAction -- from the document
    | Click Path
    | MouseDown Path
    | KeyDown Int
