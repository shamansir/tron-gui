module Tron.Control.Button exposing (..)


import Url exposing (Url)
import Color exposing (Color)
import Tron.Style.Theme exposing (Theme)

import Tron.Control as Core exposing (Control)


type Icon = Icon (Theme -> Url)


type Face
    = Default
    | WithIcon Icon
    | WithColor Color


type alias Control msg = Core.Control Face () msg


icon : Url -> Icon
icon = Icon << always


themedIcon : (Theme -> Url) -> Icon
themedIcon = Icon
