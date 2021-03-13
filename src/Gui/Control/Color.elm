module Gui.Control.Color exposing (..)


import Color exposing (Color)

import Gui.Control as Core exposing (Control)


type alias Control msg = Core.Control () Color msg
