module Tron.Render.Control.Color exposing (..)


import Bounds exposing (BoundsF)
import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Color as Color

import Tron.Style.Theme as Theme exposing (Theme)
import Tron.Render.Util exposing (State, resetTransform, describeArc, describeMark)
import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Cell as Cell

import Svg exposing (Svg)
import Svg.Attributes as SA


view : Theme -> State -> BoundsF -> Color.Control a -> Svg msg
view theme state bounds (Control _ ( _, value ) _) =
    viewValue theme state bounds value


viewValue : Theme -> State -> BoundsF -> Color -> Svg msg
viewValue theme state bounds value =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        radius = (min bounds.width bounds.height) / 6
    in Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r <| String.fromFloat radius
        , SA.fill <| Color.toCssString value
        , SA.strokeWidth "1"
        , SA.stroke <| Color.toCssString <| Coloring.lines theme state
        ]
        [
        ]