module Tron.Render.Control.Color exposing (..)


import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Color as Color

import Tron.Render.Context as Context exposing (Context)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Coloring as Coloring exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA


view : Theme -> Context -> Color.Control a -> Svg msg
view theme ctx (Control _ ( _, value ) _) =
    viewValue theme ctx value


viewValue : Theme -> Context -> Color -> Svg msg
viewValue theme ctx value =
    let
        bounds = ctx.bounds
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        radius = (min bounds.width bounds.height) / 6
    in Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r <| String.fromFloat radius
        , SA.fill <| Color.toCssString value
        , SA.strokeWidth "1"
        , SA.stroke <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
        ]
        [
        ]