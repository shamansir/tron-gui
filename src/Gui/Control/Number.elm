module Gui.Control.Number exposing (..)


import Axis exposing (Axis)

import Gui.Control as Core exposing (Control)


type alias Control msg = Core.Control Axis Float msg


