module Tron.Render.Control.Text exposing (..)


import Bounds exposing (BoundsF)
import Color as Color exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Control.Impl.Text as Text

import Tron.Style.Theme exposing (Theme)
import Tron.Render.Util exposing (State, resetTransform, describeArc, describeMark)
import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Cell as Cell

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html as Html
import Html.Attributes as HA
import Html.Events as HE


view : Theme -> State -> BoundsF -> Text.Control a -> Svg String
view theme state bounds (Control _ ( editing, value ) _) =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        fontSize = (min bounds.width bounds.height) / 6
        lineHeight = fontSize * 1.6
        topShift = cy - lineHeight / 2
        maxWidth = bounds.width - Cell.gap * 2
    in case editing of
        Text.Ready ->
            Svg.text_
                [ SA.fontSize <| String.fromFloat fontSize ++ "px"
                , SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat <| cy + 1
                , SA.class "text--ready"
                , SA.fill <| Color.toCssString <| Coloring.lines theme state
                --, SA.mask "url(#button-text-mask)"
                ]
                [ Svg.text <|
                    if String.length value <= 6 then
                        value
                    else (value |> String.left 6) ++ ".." ]
        Text.Editing ->
            Svg.g
                [ SA.style <|
                    "transform: "
                        ++ "translate(" ++ String.fromFloat Cell.gap ++ "px,"
                                        ++ String.fromFloat topShift ++ "px)"
                ]
                [ Svg.foreignObject
                    [ HA.width <| ceiling bounds.width
                    , HA.height <| ceiling bounds.height
                    , HA.style "width" <| String.fromFloat bounds.width  ++ "px"
                    , HA.style "height" <| String.fromFloat bounds.height  ++ "px"
                    , HA.style "position" "fixed"
                    , SA.class "text--edit"
                    ]
                    [ Html.input
                        [ HA.style "max-width" <| String.fromFloat maxWidth ++ "px"
                        , HA.style "height" <| String.fromFloat lineHeight ++ "px"
                        -- , HA.style "left" <| String.fromFloat Cell.gap ++ "px"
                        -- , HA.style "top" <| String.fromFloat topShift ++ "px"
                        , HA.style "font-size" <| String.fromFloat fontSize ++ "px"
                        , HA.style "color" <| Color.toCssString <| Coloring.text theme state
                        , HA.style "position" "initial"
                        , HA.type_ "text"
                        , HA.placeholder "input"
                        , HE.onInput identity
                        , HA.value value
                        ]
                        [ ]
                    ]
                ]
