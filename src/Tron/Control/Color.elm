module Tron.Control.Color exposing (..)


import Color exposing (Color)

import Tron.Control as Core exposing (Control)


type alias Control a = Core.Control () Color a
