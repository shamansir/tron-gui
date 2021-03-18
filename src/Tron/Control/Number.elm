module Tron.Control.Number exposing (..)


import Axis exposing (Axis)
import Color

import Svg exposing (Svg)
import Svg.Attributes as SA exposing (..)

import Tron.Control as Core exposing (Control)


type alias Control msg = Core.Control Axis Float msg

