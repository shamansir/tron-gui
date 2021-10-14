module Tron.Control.Action exposing (..)


import Tron.Mouse as Mouse
import Tron.Pages as Pages


type Action
    = Execute -- i.e. Click / Press Enter
    | Exit
    | Focus
    | DragStart { x : Float, y : Float }
    | Dragging { dx : Float, dy : Float }
    | DragFinish { dx : Float, dy : Float }
    | KeyDown Int
    | Select Int
    | TextInput String
    | SwitchPage Pages.PageNum


type Change
    = Silent
    | Fire
    | None