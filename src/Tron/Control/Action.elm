module Tron.Control.Action exposing (..)


import Tron.Mouse as Mouse
import Tron.Pages as Pages


type Action
    = Execute -- i.e. Click / Press Enter
    | Exit
    | Focus
    | DragStart
    | Dragging { dx : Float, dy : Float }
    | DragFinish
    | KeyDown Int
    | Select Pages.Item
    | TextInput String
    | SwitchPage Pages.Page


type Change
    = Silent
    | Fire
    | Stay