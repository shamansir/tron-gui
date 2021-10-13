module Tron.Control.Event exposing (..)


import Tron.Mouse as Mouse
import Tron.Pages as Pages


type Action
    = Execute -- i.e. Click / Press Enter
    | Focus
    | DragStart { x : Float, y : Float }
    | Dragging { dx : Float, dy : Float }
    | DragFinish { dx : Float, dy : Float }
    | KeyDown Int
    | Select Int
    | TextInput String
    | SwitchPage Pages.PageNum
