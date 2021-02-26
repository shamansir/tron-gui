module Gui.ProxyValue exposing (..)


import Color exposing (Color)
import Gui.Control.Nest exposing (Id)
import Gui.Control.Toggle exposing (ToggleState)


type ProxyValue
    = FromSlider Float
    | FromXY ( Float, Float )
    | FromInput String
    | FromChoice Id
    | FromColor Color
    | FromToggle ToggleState
    | FromButton
    | Other
