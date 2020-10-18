module Gui.Render.Plate exposing (..)


import Color
import Url exposing (Url)

import Bounds exposing (Bounds)
import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Detach exposing (ClientId, Detach, getFragment, fragmentToString)
import Gui.Property exposing (Label)

import Gui.Render.Style as Style
import Gui.Render.Property exposing (gap, borderRadius, fontFamily)
import Gui.Render.Util exposing (arrow, rotate, scale)


import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Attributes as HA
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


controls
    :  Style.Theme
    -> Style.Tone
    -> Detach msg
    -> Bounds
    -> Label
    -> Path
    -> Svg Msg
controls theme tone detachFn bounds label path =
    Svg.g
        [ ]
        [ case detachFn |> getFragment path of
            Just fragment ->
                Svg.g
                    [ SA.style <| " pointer-events: all; cursor: pointer; transform: translate(" ++
                       String.fromFloat gap ++ "px," ++ String.fromFloat gap ++ "px);"
                    , HE.onClick <| Detach path
                    ]
                    [ Svg.a
                        [ SA.xlinkHref <| fragmentToString fragment
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
        , Svg.text_
            [ SA.x <| String.fromFloat <| bounds.width / 2
            , SA.y <| String.fromFloat <| gap + 1
            , SA.fill "lightgray"
            , HA.style "dominant-baseline" "hanging"
            , HA.style "text-transform" "uppercase"
            , HA.style "font-family" fontFamily
            , HA.style "font-size" "9px"
            , HA.style "text-anchor" "middle"
            , HA.style "line-height" "3vh"
            , HA.style "letter-spacing" "1px"
            , HA.style "fill" <| Color.toCssString <| Style.text theme
            ]
            [ Svg.text label ]
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
