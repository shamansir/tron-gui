module Gui.Render.Property exposing (..)


import Gui.Property exposing (..)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Attributes as HA
import Html.Events as HE

import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Focus exposing (Focused(..))
import Gui.Render.Util exposing (..)
import Gui.Render.Style exposing (..)


view : Mode -> Tone -> Path -> Bounds -> Focused -> ( Label, Property msg ) -> Svg Msg
view mode tone path bounds focus prop =
    Svg.g
        [ HE.onClick <| Click path
        , HA.style "pointer-events" "all"
        , HA.style "cursor" "pointer"
        ]
        [ Svg.rect
            [ SA.fill <| background mode
            , SA.x "3"
            , SA.y "3"
            , SA.rx "10"
            , SA.ry "10"
            , SA.width <| String.fromFloat (bounds.width - 6) ++ "px"
            , SA.height <| String.fromFloat (bounds.height - 6) ++ "px"
            ]
            []
        ]


knobAt : Mode -> Tone -> { x : Float, y : Float } -> Float -> Svg msg
knobAt mode tone center value =
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
            [ path (colorFor tone |> toneToString)
                <| describeArc
                    center
                    { radiusA = 79, radiusB = 80 }
                    { from = toAngle 0, to = toAngle value }
            , path (knobLine mode)
                <| describeArc
                    center
                    { radiusA = 79, radiusB = 80 }
                    { from = toAngle value, to = toAngle 1 }
            , path (colorFor tone |> toneToString)
                <| describeMark
                    center
                    { radiusA = 79, radiusB = 80 }
                    (toAngle value)
            ]
