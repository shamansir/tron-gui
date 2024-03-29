module Tron.Render.Control.XY exposing (..)


import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.XY as XY

import Tron.Render.Context as Context exposing (Context)
import Tron.Render.Util exposing (resetTransform)
import Tron.Style.Theme exposing (Theme)
import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Cell as Cell

import Svg exposing (Svg)
import Svg.Attributes as SA


view : Theme -> Context -> XY.Control a -> Svg msg
view theme ctx (Control ( xAxis, yAxis ) ( _, ( valueX, valueY ) ) _) =
    let
        bounds = ctx.bounds
        ( cx, cy ) = ( bounds.width / 2, bounds.height / 2 )
        ( left, top ) = ( Cell.gap / 2, Cell.gap / 2 )
        ( right, bottom ) =
            ( bounds.width - Cell.gap / 2, bounds.height - Cell.gap / 2 )
        circleRadius = (min bounds.width bounds.height) / 18
        ( relValueX, relValueY ) =
            ( (valueX - xAxis.min) / (xAxis.max - xAxis.min)
            , (valueY - yAxis.min) / (yAxis.max - yAxis.min)
            )
        innerGap = circleRadius * 2
        ( circleX, circleY ) =
            ( left + innerGap + (relValueX * (right - left - innerGap * 2))
            , top + innerGap + (relValueY * (bottom - top - innerGap * 2))
            )
    in
    Svg.g
        [ resetTransform ]
        [ Svg.line
            [ SA.x1 <| String.fromFloat left
            , SA.y1 <| String.fromFloat cy
            , SA.x2 <| String.fromFloat right
            , SA.y2 <| String.fromFloat cy
            , SA.stroke <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
            , SA.opacity "0.2"
            , SA.strokeWidth "1"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.line
            [ SA.x1 <| String.fromFloat cx
            , SA.y1 <| String.fromFloat top
            , SA.x2 <| String.fromFloat cx
            , SA.y2 <| String.fromFloat bottom
            , SA.stroke <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
            , SA.opacity "0.2"
            , SA.strokeWidth "1"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ SA.cx <| String.fromFloat circleX
            , SA.cy <| String.fromFloat circleY
            , SA.fill <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
            , SA.fill "none"
            , SA.stroke <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
            , SA.strokeWidth "2"
            , SA.r <| String.fromFloat circleRadius
            ]
            []
        , Svg.text_
            [ SA.x <| String.fromFloat <| circleX + circleRadius * 1.5
            , SA.y <| String.fromFloat <| circleY + circleRadius * 1.5
            , SA.fill <| Color.toCssString <| Coloring.lines theme <| Context.styleDef ctx
            --, SA.fill <| Color.toCssString <| Coloring.text theme <| Context.styleDef ctx
            , SA.fontSize "7px"
            , SA.style "pointer-events: none"
            ]
            [ Svg.text <| "(" ++ String.fromFloat valueX ++ "," ++ String.fromFloat valueY ++ ")" ]
        ]
-- 5 30 10 30 10 30 10 30 5
