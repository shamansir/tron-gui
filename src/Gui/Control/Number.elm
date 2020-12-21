module Gui.Control.Number exposing (..)


import Axis exposing (Axis)
import Color

import Svg exposing (Svg)
import Svg.Attributes as SA exposing (..)

import Gui.Control as Core exposing (Control)


type alias Control msg = Core.Control Axis Float msg

