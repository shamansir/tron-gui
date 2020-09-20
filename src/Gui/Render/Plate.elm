module Gui.Render.Plate exposing (..)


import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))

import Gui.Render.Style as Style
import Gui.Render.Property exposing (gap, borderRadius)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Events as HE


back : Style.Mode -> Bounds -> Svg Msg
back style bounds =
    Svg.rect
        [ SA.fill <| Style.background style
        , SA.x <| String.fromFloat (gap / 2)
        , SA.y <| String.fromFloat (gap / 2)
        , SA.rx <| String.fromFloat borderRadius
        , SA.ry <| String.fromFloat borderRadius
        , SA.width <| String.fromFloat (bounds.width - gap) ++ "px"
        , SA.height <| String.fromFloat (bounds.height - gap) ++ "px"
        ]
        []


controls : Style.Tone -> Bounds -> Path -> Svg Msg
controls tone bounds path =
    Svg.g
        [ SA.style <| " pointer-events: all; cursor: pointer; transform: translate(" ++
            (String.fromFloat <| bounds.width - gap)
            ++ "px," ++ String.fromFloat gap ++ "px);"
        , HE.onClick <| Click path
        ]
        [ Svg.rect
            [ SA.fill "transparent"
            , SA.x "-11"
            , SA.y "2.5"
            , SA.width "10"
            , SA.height "10"
            ]
            []
        , arrow tone
        ]


arrow : Style.Tone -> Svg msg
arrow tone =
    Svg.g
        [ SA.style "transform: scale(0.35) translate(15px,52px) rotate(180deg);"
        ]
        [ Svg.polyline
            [ SA.fill <| Style.toneToString <| Style.colorFor tone
            , SA.points "18.3,32.5 32,18.8 45.7,32.5 43.8,34.3 32,22.6 20.2,34.3 18.3,32.5"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.polygon
            [ SA.fill <| Style.toneToString <| Style.colorFor tone
            , SA.points "33.4,20.7 33.4,44.7 30.6,44.7 30.6,20.7"
            , SA.strokeLinecap "round"
            ]
            []
        ]
