module Tron.Control.Impl.Number exposing (..)


import Axis exposing (Axis)
import Color

import Svg exposing (Svg)
import Svg.Attributes as SA exposing (..)

import Tron.Control as Core exposing (Control)

-- if the slider is being dragged now, we need to know its first value it had when user started dragging,
-- so it is the first `Maybe` in the pair
type alias Control a = Core.Control Axis ( Maybe Float, Float ) a
