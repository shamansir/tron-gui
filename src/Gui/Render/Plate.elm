module Gui.Render.Plate exposing (..)


import Color
import Url exposing (Url)

import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Detach exposing (DetachFn, callWith)

import Gui.Render.Style as Style
import Gui.Render.Property exposing (gap, borderRadius)
import Gui.Render.Util exposing (arrow, rotate, scale)


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
                    [ Svg.a
                        [ SA.xlinkHref <| Url.toString url
                        , SA.target "_blank"
                        ]
                        [ Svg.rect
                            [ SA.fill "transparent"
                            , SA.x "0"
                            , SA.y "2.5"
                            , SA.width "10"
                            , SA.height "10"
                            ]
                            []
                        , detach theme tone
                        ]
                    ]
            Nothing -> Svg.g [] []
        , Svg.g
            [ SA.style <| " pointer-events: all; cursor: pointer; transform: translate(" ++
                (String.fromFloat <| bounds.width - gap - 10)
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
    arrow (Style.colorFor theme tone) (scale 0.35) (rotate 45)


collapse : Style.Theme -> Style.Tone -> Svg msg
collapse theme tone =
    arrow (Style.colorFor theme tone) (scale 0.35) (rotate 180)
