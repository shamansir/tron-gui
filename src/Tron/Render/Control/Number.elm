module Tron.Render.Control.Number exposing (..)


import Bounds exposing (BoundsF)
import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Number as Number

import Tron.Style.Theme exposing (Theme)
import Tron.Render.Context as Context exposing (Context)
import Tron.Render.Util exposing (resetTransform, describeArc, describeMark)
import Tron.Style.Coloring as Coloring exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA


view : Theme -> Context -> Number.Control a -> Svg msg
view theme ctx (Control { min, max } ( _, srcValue ) _)  =
    let
        style = Context.styleDef ctx
        bounds = ctx.bounds
        value = if srcValue < min then min
                   else if srcValue > max then max
                   else srcValue
        relValue = if srcValue < min then 0
                   else if srcValue > max then 1
                   else (srcValue - min) / (max - min)
        toAngle v = (-120) + (v * 120 * 2)
        minAngle = toAngle 0
        maxAngle =toAngle 1
        -- FIXME: move aligning the value to control
        path stroke d =
            Svg.path
                [ SA.d d
                , SA.fill "none"
                , SA.stroke stroke
                , SA.strokeWidth "2"
                , SA.strokeLinecap "round"
                ]
                []
        radiusA = (bounds.width * 0.27) - 1
        radiusB = bounds.height * 0.27
        ( cx, cy ) = ( bounds.width / 2, bounds.height / 2 )
        roundedValue = Basics.toFloat (floor (value * 100)) / 100
        valueAngle =
            if relValue > 1 then maxAngle else
            if relValue < 0 then minAngle
            else toAngle relValue
    in
        Svg.g
            [ resetTransform ]
            [ path (Coloring.lines theme style |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = minAngle
                    , to = valueAngle
                    }
            , path (Coloring.secondaryLines theme style |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = valueAngle
                    , to = maxAngle
                    }
            , path (Coloring.lines theme style |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    valueAngle
            , Svg.text_
                [ SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat cy
                , SA.class "knob__value"
                , SA.style "pointer-events: none"
                , SA.fill <| Color.toCssString <| Coloring.text theme style
                ]
                [ Svg.text <| String.fromFloat roundedValue ]
            ]