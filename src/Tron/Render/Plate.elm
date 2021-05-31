module Tron.Render.Plate exposing (..)


import Color
import Url exposing (Url)
import Bounds exposing (..)

import Tron.Path exposing (Path)
import Tron.Msg exposing (Msg_(..))
import Tron.Pages as Pages
import Tron.Detach as Detach exposing (Ability(..), ClientId, localUrlToString, LocalUrl)
import Tron.Property exposing (Label, Property)

import Tron.Render.Transform exposing (rotate, scale)
import Tron.Render.Util exposing (arrow, State)
import Tron.Render.Util as Svg exposing (none)

import Tron.Focus exposing (Focused(..))
import Tron.Style.Logic exposing (..)
import Tron.Style.Selected exposing (Selected(..))
import Tron.Style.Placement exposing (Placement(..))
import Tron.Style.Coloring as Coloring
import Tron.Style.Cell as Cell
import Tron.Style.CellShape as CS
import Tron.Style.Theme as Theme exposing (Theme)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html.Attributes as HA
import Html.Events as HE


state : State
state = ( AtRoot, NotFocused, Usual )


back : Theme -> BoundsF -> Svg Msg_
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
    :  Detach.Ability
    -> Theme
    -> Path
    -> BoundsF
    -> ( Label, Property a )
    -> Svg Msg_
controls detachAbility theme path bounds ( label, prop ) =
    Svg.g
        [ SA.class "plate-controls" ]
        [ case detachAbility of
            CanBeDetached localUrl ->
                detachButton
                    theme
                    path
                    localUrl
                    ( Cell.gap, Cell.gap )
            CannotBeDetached -> Svg.none
        , let
            labelLength = String.length label
            rectWidth =  Basics.toFloat labelLength * 7.5
            rectHeight = 12
        in
            Svg.rect
                [ SA.x <| String.fromFloat <| (bounds.width / 2) - (rectWidth / 2)
                , SA.y <| String.fromFloat <| Cell.gap - 2
                , SA.rx "5"
                , SA.ry "5"
                , SA.width <| String.fromFloat rectWidth
                , SA.height <| String.fromFloat rectHeight
                , SA.fill <| Color.toCssString <| Coloring.titleBack theme state
                -- , SA.strokeWidth "1"
                -- , SA.stroke "white"
                ]
                [
                ]
        , Svg.text_
            [ SA.class "plate-controls__title"
            , SA.x <| String.fromFloat <| bounds.width / 2
            , SA.y <| String.fromFloat <| Cell.gap + 1
            , SA.fill <| Color.toCssString <| Coloring.title theme state
            ]
            [ Svg.text label ]
        {- , collapseButton
            theme
            path
            ( bounds.width - Cell.gap - 10, Cell.gap ) -}
        ]


paging
     : Theme
    -> Path
    -> BoundsF
    -> CS.CellShape
    -> ( Pages.PageNum, Pages.Count )
    -> Svg Msg_
paging _ path bounds cellShape ( current, total ) =
    let
        itemWidth = bounds.width / Basics.toFloat total
        cellWidth = (CS.numify cellShape |> Tuple.first) * Cell.width
        backgroundRect =
            Svg.rect
            [ SA.x <| String.fromFloat 5
            , SA.y <| String.fromFloat <| bounds.height - 35
            , SA.rx <| String.fromFloat <| 10.0
            , SA.ry <| String.fromFloat <| 10.0
            , SA.width <| String.fromFloat <| bounds.width - 10
            , SA.height <| String.fromFloat 30.0
            , SA.style "pointer-events: none; cursor: pointer;"
            , SA.fill "black"--"rgba(200,200,200,0.3)"
            ]
            []
        pageRect page =
            Svg.rect
                [ SA.x <| String.fromFloat <| (Basics.toFloat page * itemWidth) + cellWidth / Basics.toFloat total
                , SA.y <| String.fromFloat <| bounds.height - 11
                , SA.rx <| String.fromFloat <| 2.0
                , SA.ry <| String.fromFloat <| 2.0
                , SA.width <| String.fromFloat <| cellWidth / Basics.toFloat total -- itemWidth / 4
                , SA.height <| String.fromFloat 4.0
                , SA.style "pointer-events: all; cursor: pointer;"
                , HE.onClick <| SwitchPage path page
                , SA.fill <| if page == current then "white" else "gray"
                ]
                [ ]
        switchingRect page =
            Svg.rect
                [ SA.x <| String.fromFloat <| Basics.toFloat page * itemWidth
                , SA.y <| String.fromFloat <| bounds.height - 20
                , SA.width <| String.fromFloat itemWidth
                , SA.height <| String.fromFloat 20
                , SA.style "pointer-events: all; cursor: pointer;"
                , HE.onClick <| SwitchPage path page
                , SA.fill "transparent"
                ]
                [ ]

    in
    Svg.g
        []
        [ backgroundRect
        , Svg.g
            []
            <| List.map
            (\page ->
                Svg.g
                    []
                    [ pageRect page
                    , switchingRect page
                    ]
            )
            <| List.range 0 (total - 1)
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
