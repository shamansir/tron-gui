module Tron.Render.Control.Toggle exposing (..)

import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Toggle as Toggle

import Tron.Render.Context exposing (Context)
import Tron.Style.Theme as Theme exposing (Theme)

import Svg exposing (Svg)
import Svg.Attributes as SA


view : Theme -> Context -> Toggle.Control a -> Svg msg
view theme ctx (Control _ tstate _) =
    let
        bounds = ctx.bounds
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) )
        ( rectX, rectY ) = ( cx - 36 / 2, cy - 20 / 2 )
        circleRadius = 8
        rectCornerRadius = 10
        ( rectWidth, rectHeight ) = ( 36, 20 )
        ( circleX, circleY ) =
            ( case tstate of
                Toggle.TurnedOff -> rectX + 2 + circleRadius
                Toggle.TurnedOn -> rectX + rectWidth - 2 - circleRadius
            , cy
            )
    in
        Svg.g
            []
            [ Svg.rect
                [ SA.x <| String.fromFloat rectX
                , SA.y <| String.fromFloat rectY
                , SA.rx <| String.fromFloat rectCornerRadius
                , SA.ry <| String.fromFloat rectCornerRadius
                , SA.width <| String.fromFloat rectWidth
                , SA.height <| String.fromFloat rectHeight
                , SA.fill <| case (theme, tstate) of
                    (Theme.Dark, Toggle.TurnedOn) -> Color.toCssString Color.white
                    (Theme.Dark, Toggle.TurnedOff) -> Color.toCssString <| Color.rgba 1 1 1 0.4
                    (Theme.Light, Toggle.TurnedOn) -> Color.toCssString Color.black
                    (Theme.Light, Toggle.TurnedOff) -> Color.toCssString <| Color.rgba 0 0 0 0.2
                ]
                [ ]
            , Svg.circle
                [ SA.cx <| String.fromFloat circleX
                , SA.cy <| String.fromFloat circleY
                , SA.r <| String.fromFloat 8
                , SA.fill <| case theme of
                  Theme.Dark -> Color.toCssString Color.black
                  Theme.Light -> Color.toCssString Color.white
                ]
                []
            ]
