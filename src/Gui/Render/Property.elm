module Gui.Render.Property exposing (..)


import Gui.Property exposing (..)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Attributes as HA
import Html.Events as HE

import Bounds exposing (Bounds)
import Gui.Path as Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Focus exposing (Focused(..))
import Gui.Control exposing (Control(..))
import Gui.Render.Util exposing (..)
import Gui.Render.Util as Svg exposing (none)
import Gui.Render.Style exposing (..)


gap = 6


borderRadius = 10


view : Mode -> Tone -> Path -> Bounds -> Focused -> ( Label, Property msg ) -> Svg Msg
view mode tone path bounds focus ( label, prop ) =
    Svg.g
        [ HE.onClick <| Click path
        , HA.style "pointer-events" "all"
        , HA.style "cursor" "pointer"
        ]
        [
            if (Path.howDeep path == 1) then
                Svg.rect
                    [ SA.fill <| background mode
                    , SA.x <| String.fromFloat (gap / 2)
                    , SA.y <| String.fromFloat (gap / 2)
                    , SA.rx <| String.fromFloat borderRadius
                    , SA.ry <| String.fromFloat borderRadius
                    , SA.width <| String.fromFloat (bounds.width - gap) ++ "px"
                    , SA.height <| String.fromFloat (bounds.height - gap) ++ "px"
                    ]
                    []
            else Svg.none
        ,
            case prop of
                Number (Control { min, max } value _) ->
                    knob
                        mode
                        tone
                        { x = cellWidth / 2, y = (cellHeight / 2) }
                        <| (value - min) / (max - min)
                Toggle (Control _ value _) ->
                    toggle mode tone value { x = cellWidth / 2, y = (cellHeight / 2) - 3 }
                Action (Control maybeIcon _ _) ->
                    button mode tone maybeIcon { x = cellWidth / 2, y = (cellHeight / 2) - 3 }
                Choice (Control _ ( expanded, _ ) _) ->
                    arrow mode tone expanded { x = cellWidth / 2, y = (cellHeight / 2) - 3 }
                Group (Control _ ( expanded, _ ) _) ->
                    arrow mode tone expanded { x = cellWidth / 2, y = (cellHeight / 2) - 3 }
                _ -> Svg.none
        , Svg.text_
            [ SA.textAnchor "middle"
            , SA.dominantBaseline "middle"
            , SA.x <| String.fromFloat (cellWidth / 2)
            , SA.y <| String.fromFloat (cellWidth / 5 * 4)
            , SA.fontSize "13px"
            , SA.fontFamily "\"IBM Plex Sans\", sans-serif"
            , SA.fill "rgb(144, 144, 144)"
            ]
            [ Svg.text label ]
        ]


knob : Mode -> Tone -> { x : Float, y : Float } -> Float -> Svg msg
knob mode tone center value =
    let
        toAngle v = (-120) + (v * 120 * 2)
        path color d =
            Svg.path
                [ SA.d d
                , SA.fill "none"
                , SA.stroke color
                , SA.strokeWidth "2.5"
                , SA.strokeLinecap "round"
                ]
                []
        radiusA = radiusB - 1
        radiusB = cellWidth * 0.27
    in
        Svg.g
            []
            [ path (colorFor tone |> toneToString)
                <| describeArc
                    center
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle 0, to = toAngle value }
            , path (knobLine mode)
                <| describeArc
                    center
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle value, to = toAngle 1 }
            , path (colorFor tone |> toneToString)
                <| describeMark
                    center
                    { radiusA = radiusA, radiusB = radiusB }
                    (toAngle value)
            ]


arrow : Mode -> Tone -> ExpandState -> { x : Float, y : Float } -> Svg msg
arrow mode tone expand center =
    Svg.g
        [ SA.style <| "transform: translate("
            ++ String.fromFloat center.x ++ "px,"
            ++ String.fromFloat center.y ++ "px)"
            ++ case expand of
                Expanded -> " rotate(180deg);"
                Collapsed -> ";"
        ]
        [ Svg.g
            [ SA.style "transform: scale(0.7) translate(-32px,-32px);" ]
            [ Svg.polyline
                [ SA.fill <| toneToString <| colorFor tone
                , SA.points "18.3,32.5 32,18.8 45.7,32.5 43.8,34.3 32,22.6 20.2,34.3 18.3,32.5"
                ]
                []
            , Svg.polygon
                [ SA.fill <| toneToString <| colorFor tone
                , SA.points "33.4,20.7 33.4,44.7 30.6,44.7 30.6,20.7"
                ]
                []
            ]
        ]


button : Mode -> Tone -> Maybe Icon -> { x : Float, y : Float } -> Svg msg
button mode tone maybeIcon center =
    case maybeIcon of
        Just icon -> Svg.image [] []
        Nothing -> toggle mode tone TurnedOff center



toggle : Mode -> Tone -> ToggleState -> { x : Float, y : Float } -> Svg msg
toggle mode tone state center =
    Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r "10"
        , SA.fill <| case state of
            TurnedOn -> toneToString <| colorFor tone
            TurnedOff -> "none"
        , SA.stroke <| toneToString <| colorFor tone
        , SA.strokeWidth "2.5"
        ]
        [
        ]
