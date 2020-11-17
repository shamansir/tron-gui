module Gui.Control.XY exposing (..)


import Axis exposing (Axis)

import Gui.Control as Core exposing (Control)


type alias Control msg = Core.Control ( Axis, Axis ) ( Float, Float ) msg


