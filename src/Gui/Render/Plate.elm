module Gui.Render.Plate exposing (..)


import Color
import Url exposing (Url)

import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Detach exposing (DetachFn, callWith)

import Gui.Render.Style as Style
import Gui.Render.Property exposing (gap, borderRadius)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Events as HE


back : Style.Theme -> Bounds -> Svg Msg
back theme bounds =
    Svg.rect
        [ SA.fill <| Color.toCssString <| Style.background theme
        , SA.x <| String.fromFloat (gap / 2)
        , SA.y <| String.fromFloat (gap / 2)
        , SA.rx <| String.fromFloat borderRadius
        , SA.ry <| String.fromFloat borderRadius
        , SA.width <| String.fromFloat (bounds.width - gap) ++ "px"
        , SA.height <| String.fromFloat (bounds.height - gap) ++ "px"
        ]
        []


controls : Style.Theme -> Style.Tone -> DetachFn -> Bounds -> Path -> Svg Msg
controls theme tone detachFn bounds path =
    Svg.g
        [ ]
        [ case detachFn |> callWith path of
            Just url ->
                Svg.g
                    [ SA.style <| " pointer-events: all; cursor: pointer; transform: translate(" ++
                       String.fromFloat gap ++ "px," ++ String.fromFloat gap ++ "px);"
                    , HE.onClick <| Detach path
                    ]
                    [ Svg.rect
                        [ SA.fill "transparent"
                        , SA.x "-11"
                        , SA.y "2.5"
                        , SA.width "10"
                        , SA.height "10"
                        ]
                        []
                    , Svg.a
                        [ SA.xlinkHref <| Url.toString url
                        , SA.target "_blank"
                        ]
                        [ detach theme tone ]
                    ]
            Nothing -> Svg.g [] []
        , Svg.g
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
            , collapse theme tone
            ]
        ]


detach : Style.Theme -> Style.Tone -> Svg msg
detach theme tone =
    Svg.g
        [ SA.style "transform: scale(0.35) translate(15px,52px) rotate(45deg);"
        ]
        [ Svg.polyline
            [ SA.fill <| Color.toCssString <| Style.colorFor theme tone
            , SA.points "18.3,32.5 32,18.8 45.7,32.5 43.8,34.3 32,22.6 20.2,34.3 18.3,32.5"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.polygon
            [ SA.fill <| Color.toCssString <| Style.colorFor theme tone
            , SA.points "33.4,20.7 33.4,44.7 30.6,44.7 30.6,20.7"
            , SA.strokeLinecap "round"
            ]
            []
        ]


collapse : Style.Theme -> Style.Tone -> Svg msg
collapse theme tone =
    Svg.g
        [ SA.style "transform: scale(0.35) translate(15px,52px) rotate(180deg);"
        ]
        [ Svg.polyline
            [ SA.fill <| Color.toCssString <| Style.colorFor theme tone
            , SA.points "18.3,32.5 32,18.8 45.7,32.5 43.8,34.3 32,22.6 20.2,34.3 18.3,32.5"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.polygon
            [ SA.fill <| Color.toCssString <| Style.colorFor theme tone
            , SA.points "33.4,20.7 33.4,44.7 30.6,44.7 30.6,20.7"
            , SA.strokeLinecap "round"
            ]
            []
        ]
