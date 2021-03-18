module Tron.Control.Button exposing (..)


import Color exposing (Color)

import Tron.Control as Core exposing (Control)


type Icon = Icon String


type Face
    = Default
    | WithIcon Icon
    | WithColor Color


type alias Control msg = Core.Control Face () msg


icon : String -> Icon
icon = Icon
