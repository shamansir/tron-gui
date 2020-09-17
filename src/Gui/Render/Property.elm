module Gui.Render.Property exposing (..)


import Gui.Property exposing (..)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Events as HE

import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Focus exposing (Focused(..))
import Gui.Render.Util exposing (..)
import Gui.Render.Style exposing (..)


view : Mode -> Style -> Path -> Bounds -> Focused -> ( Label, Property msg ) -> Svg Msg
view mode style path bounds focus prop =
    Svg.g
        [ HE.onClick <| Click path
        ]
        [ Svg.rect
            [ SA.fill <| background mode
            , SA.rx "10"
            , SA.ry "10"
            , SA.width <| String.fromFloat bounds.width ++ "px"
            , SA.height <| String.fromFloat bounds.height ++ "px"
            ]
            []
        ]


knobAt : Mode -> Style -> { x : Float, y : Float } -> Float -> Svg msg
knobAt mode style center value =
    let
        toAngle v = (-120) + (v * 120 * 2)
        path color d =
            Svg.path
                [ SA.d d
                , SA.fill "none"
                , SA.stroke color
                , SA.strokeWidth "3"
                , SA.strokeLinecap "round"
                ]
                []
    in
        Svg.g
            []
            [ path (colorFor style |> colorToString)
                <| describeArc
                    center
                    { radiusA = 79, radiusB = 80 }
                    { from = toAngle 0, to = toAngle value }
            , path (knobLine mode)
                <| describeArc
                    center
                    { radiusA = 79, radiusB = 80 }
                    { from = toAngle value, to = toAngle 1 }
            , path (colorFor style |> colorToString)
                <| describeMark
                    center
                    { radiusA = 79, radiusB = 80 }
                    (toAngle value)
            ]
