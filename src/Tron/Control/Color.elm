module Tron.Control.Color exposing (..)


import Color exposing (Color)

import Tron.Control as Core exposing (Control)


type alias Control msg = Core.Control () Color msg
