module Gui.Msg exposing (..)


import Gui.Path exposing (Path)
import Gui.Mouse exposing (MouseAction)
import Gui.Expose as Exp


type Msg
    = NoOp
    | ApplyMouse MouseAction -- from the document
    -- | Reflow ( Int, Int ) -- TODO:
    | Click Path
    | MouseDown Path
    | KeyDown Int
    | Detach Path
    | RefreshRaw Exp.RawProperty
    | ReceiveRaw Exp.RawUpdate
