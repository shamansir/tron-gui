module Gui.Render.Plate exposing (..)


import Color
import Url exposing (Url)
import BinPack exposing (Bounds)

import Gui.Path exposing (Path)
import Gui.Msg exposing (Msg_(..))
import Gui.Detach exposing (ClientId, Detach, getLocalUrl, localUrlToString, LocalUrl)
import Gui.Property exposing (Label, Property)

import Gui.Render.Transform exposing (rotate, scale)
import Gui.Render.Util exposing (arrow, State)
import Gui.Render.Util as Svg exposing (none)

import Gui.Focus exposing (Focused(..))
import Gui.Style.Logic exposing (..)
import Gui.Style.Selected exposing (Selected(..))
import Gui.Style.Placement exposing (Placement(..))
import Gui.Style.Coloring as Coloring
import Gui.Style.Cell as Cell
import Gui.Style.Theme exposing (Theme)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Attributes as HA
import Html.Events as HE


state : State
state = ( AtRoot, NotFocused, Usual )


back : Theme -> Bounds -> Svg Msg_
back theme bounds =
    Svg.rect
        [ SA.fill <| Color.toCssString <| Coloring.back theme state
        , SA.x <| String.fromFloat (Cell.gap / 2)
        , SA.y <| String.fromFloat (Cell.gap / 2)
        , SA.rx <| String.fromFloat Cell.borderRadius
        , SA.ry <| String.fromFloat Cell.borderRadius
        , SA.width <| String.fromFloat (bounds.width - Cell.gap) ++ "px"
        , SA.height <| String.fromFloat (bounds.height - Cell.gap) ++ "px"
        ]
        []


controls
    :  Detach msg
    -> Theme
    -> Path
    -> Bounds
    -> ( Label, Property msg )
    -> Svg Msg_
controls detachFn theme path bounds ( label, prop ) =
    Svg.g
        [ SA.class "plate-controls" ]
        [ case detachFn |> getLocalUrl path of
            Just localUrl ->
                detachButton
                    theme
                    path
                    localUrl
                    ( Cell.gap, Cell.gap )
            Nothing -> Svg.none
        , Svg.text_
            [ SA.class "plate-controls__title"
            , SA.x <| String.fromFloat <| bounds.width / 2
            , SA.y <| String.fromFloat <| Cell.gap + 1
            , SA.fill <| Color.toCssString <| Coloring.text theme state
            ]
            [ Svg.text label ]
        , collapseButton
            theme
            path
            ( bounds.width - Cell.gap - 10, Cell.gap )
        ]


detach : Theme -> Svg msg
detach theme =
    arrow (Coloring.lines theme state) (scale 0.35) (rotate 45)


collapse : Theme -> Svg msg
collapse theme =
    arrow (Coloring.lines theme state) (scale 0.35) (rotate 180)


collapseButton : Theme -> Path -> ( Float, Float ) -> Svg Msg_
collapseButton theme path (x, y) =
    Svg.g
        [ SA.class "collapse-panel"
        , SA.style <| "transform: translate(" ++
            String.fromFloat x ++ "px,"
            ++ String.fromFloat y ++ "px);"
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
        , collapse theme
        ]


detachButton : Theme -> Path -> LocalUrl -> ( Float, Float ) -> Svg Msg_
detachButton theme path localUrl (x, y) =
    Svg.g
        [ SA.class "detach-panel"
        , SA.style <| "transform: translate(" ++
        String.fromFloat x ++ "px," ++ String.fromFloat y ++ "px);"
        , HE.onClick <| Detach path
        ]
        [ Svg.a
            [ SA.xlinkHref <| localUrlToString localUrl
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
            , detach theme
            ]
        ]
