module Gui.Render.Property exposing (..)


import Color exposing (Color)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html
import Html.Attributes as HA
import Html.Events as HE

import Bounds exposing (Bounds)

import Gui.Property exposing (..)
import Gui.Path as Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Focus exposing (Focused(..))
import Gui.Control exposing (Control(..))

import Gui.Control.Text exposing (TextState(..))
import Gui.Control.Button exposing (Face(..), Icon(..))
import Gui.Control.Toggle exposing (ToggleState(..))
import Gui.Control.Nest as Nest exposing (getState, NestState(..))

import Gui.Render.Transform exposing (..)
import Gui.Render.Util exposing (..)
import Gui.Render.Util as Svg exposing (none)
import Gui.Render.Util as Util exposing (arrow)

import Gui.Style.Logic exposing (..)
import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CS exposing (..)
import Gui.Style.Tone exposing (Tone)
import Gui.Style.Tone as Tone exposing (..)
import Gui.Style.Theme exposing (Theme)
import Gui.Style.Theme as Theme exposing (toString)
import Gui.Style.Placement exposing (Placement)
import Gui.Style.Selected exposing (Selected(..))
import Gui.Style.Cell as Cell


view
     : Style
    -> State
    -> Path
    -> Bounds
    -> Maybe ( Label, Property msg )
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg
view ( ( theme, tone ) as style ) state path bounds maybeSelectedInside cellShape ( label, prop ) =
    Svg.g
        [ HE.onClick <| Click path
        , SA.class <| makeClass tone cellShape <| prop
        ]
        [ Svg.rect
            [ SA.fill
                <| Color.toCssString
                <| Tone.back style state
            , SA.x <| String.fromFloat (Cell.gap / 2)
            , SA.y <| String.fromFloat (Cell.gap / 2)
            , SA.rx <| String.fromFloat Cell.borderRadius
            , SA.ry <| String.fromFloat Cell.borderRadius
            , SA.width <| String.fromFloat (bounds.width - Cell.gap) ++ "px"
            , SA.height <| String.fromFloat (bounds.height - Cell.gap) ++ "px"
            ]
            []
        , viewProperty
            style state path bounds maybeSelectedInside cellShape ( label, prop )
        , case prop of
            Action _ -> Svg.none
            _ -> viewLabel style state cellShape bounds label
        ]


viewLabel
    :  Style
    -> State
    -> CellShape
    -> Bounds
    -> Label
    -> Svg msg
viewLabel style state cellShape bounds label =
    if CS.isSquare cellShape
        then
            Svg.text_
                [ SA.class "cell__label"
                , SA.x <| String.fromFloat (bounds.width / 2)
                , SA.y <| String.fromFloat (bounds.height / 5 * 4)
                , SA.fill <| Color.toCssString <| Tone.text style state
                ]
                [ Svg.text label ]
        else Svg.none


viewProperty
     : Style
    -> State
    -> Path
    -> Bounds
    -> Maybe ( Label, Property msg )
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg
viewProperty
    style
    ( ( placement, focus, selected ) as state )
    path
    bounds
    maybeSelectedInside
    cellShape
    ( label, prop ) =
    case prop of
        Number (Control { min, max } value _) ->
            knob
                style
                state
                bounds
                <| (value - min) / (max - min)
        Coordinate (Control ( xAxis, yAxis ) ( xValue, yValue ) _) ->
            coord
                style
                state
                bounds
                <|
                    ( (xValue - xAxis.min) / (xAxis.max - xAxis.min)
                    , (yValue - yAxis.min) / (yAxis.max - yAxis.min)
                    )
        Text (Control _ value _) ->
            text style state value (TextInput path) bounds
        Toggle (Control _ value _) ->
            toggle style state value bounds
        Action (Control face _ _) ->
            button style state face cellShape label bounds
        Color (Control _ value _) ->
            color style state value bounds
        Choice _ _ control ->
            case maybeSelectedInside of
                Just theSelectedProp ->
                    viewProperty
                        style
                        ( placement, focus, Selected )
                        path
                        bounds
                        Nothing
                        cellShape
                        theSelectedProp
                Nothing ->
                    arrow style state (Nest.getState control) bounds
        Group _ _ control ->
            arrow style state (Nest.getState control) bounds
        _ -> Svg.none


knob : Style -> State -> Bounds -> Float -> Svg msg
knob style state bounds value =
    let
        toAngle v = (-120) + (v * 120 * 2)
        path stroke d =
            Svg.path
                [ SA.d d
                , SA.fill "none"
                , SA.stroke stroke
                , SA.strokeWidth "2"
                , SA.strokeLinecap "round"
                ]
                []
        radiusA = (bounds.width * 0.27) - 1
        radiusB = bounds.height * 0.27
        ( cx, cy ) = ( bounds.width / 2, bounds.height / 2 )
    in
        Svg.g
            []
            [ path (Tone.lines style state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle 0, to = toAngle value }
            , path (Tone.secondaryLines style state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle value, to = toAngle 1 }
            , path (Tone.lines style state |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    (toAngle value)
            ]


coord : Style -> State -> Bounds -> ( Float, Float ) -> Svg msg
coord style state bounds ( valueX, valueY ) =
    let
        ( cx, cy ) = ( bounds.width / 2, bounds.height / 2 )
        ( left, top ) = ( Cell.gap / 2, Cell.gap / 2 )
        ( right, bottom ) =
            ( bounds.width - Cell.gap / 2, bounds.height - Cell.gap / 2 )
        circleRadius = (min bounds.width bounds.height) / 18
        innerGap = circleRadius * 2
        ( circleX, circleY ) =
            ( left + innerGap + (valueX * (right - left - innerGap * 2))
            , top + innerGap + (valueY * (bottom - top - innerGap * 2))
            )
    in
    Svg.g
        []
        [ Svg.line
            [ SA.x1 <| String.fromFloat left
            , SA.y1 <| String.fromFloat cy
            , SA.x2 <| String.fromFloat right
            , SA.y2 <| String.fromFloat cy
            , SA.stroke <| Color.toCssString <| Tone.lines style state
            , SA.opacity "0.2"
            , SA.strokeWidth "1"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.line
            [ SA.x1 <| String.fromFloat cx
            , SA.y1 <| String.fromFloat top
            , SA.x2 <| String.fromFloat cx
            , SA.y2 <| String.fromFloat bottom
            , SA.stroke <| Color.toCssString <| Tone.lines style state
            , SA.opacity "0.2"
            , SA.strokeWidth "1"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ SA.cx <| String.fromFloat circleX
            , SA.cy <| String.fromFloat circleY
            , SA.fill <| Color.toCssString <| Tone.lines style state
            , SA.fill "none"
            , SA.stroke <| Color.toCssString <| Tone.lines style state
            , SA.strokeWidth "2"
            , SA.r <| String.fromFloat circleRadius
            ]
            []
        ]
-- 5 30 10 30 10 30 10 30 5


text
    :  Style
    -> State
    -> ( TextState, String )
    -> (String -> msg)
    -> Bounds
    -> Svg msg
text style state ( editing, value ) onInput bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        fontSize = (min bounds.width bounds.height) / 6
        lineHeight = fontSize * 1.6
        topShift = cy - lineHeight / 2
        maxWidth = bounds.width - Cell.gap * 2
    in case editing of
        Ready ->
            Svg.text_
                [ SA.fontSize <| String.fromFloat fontSize ++ "px"
                , SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat <| cy + 1
                , SA.class "text--ready"
                , SA.fill <| Color.toCssString <| Tone.lines style state
                ]
                [ Svg.text <|
                    if String.length value <= 6 then
                        value
                    else (value |> String.left 6) ++ ".." ]
        Editing ->
            Svg.foreignObject
                [ HA.style "width" <| String.fromFloat bounds.width  ++ "px"
                , HA.style "height" <| String.fromFloat bounds.height  ++ "px"
                , SA.class "text--edit"
                ]
                [ Html.input
                    [ HA.style "max-width" <| String.fromFloat maxWidth ++ "px"
                    , HA.style "height" <| String.fromFloat lineHeight ++ "px"
                    , HA.style "left" <| String.fromFloat Cell.gap ++ "px"
                    , HA.style "top" <| String.fromFloat topShift ++ "px"
                    , HA.style "font-size" <| String.fromFloat fontSize ++ "px"
                    , HA.style "color" <| Color.toCssString <| Tone.text style state
                    , HA.type_ "text"
                    , HA.placeholder "input"
                    , HE.onInput onInput
                    , HA.value value
                    ]
                    [ ]
                ]


toggle : Style -> State -> ToggleState -> Bounds -> Svg msg
toggle style state tstate bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        radius = ( min bounds.width bounds.height) / 7
    in Svg.circle
        [ SA.cx <| String.fromFloat cx
        , SA.cy <| String.fromFloat cy
        , SA.r <| String.fromFloat radius
        , SA.fill <| case tstate of
            TurnedOn -> Color.toCssString <| Tone.lines style state
            TurnedOff -> "none"
        , SA.stroke <| Color.toCssString <| Tone.lines style state
        , SA.strokeWidth "2"
        ]
        [
        ]


button : Style -> State -> Face -> CellShape -> Label -> Bounds -> Svg msg
button ( ( theme, tone ) as style ) ( ( _, _, selected ) as state ) face cellShape label bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        ( labelX, labelY ) =
            if CS.isHorizontal cellShape
                then ( 30, cy + 2 )
                else ( cx, cy )
        textLabel _ =
            Svg.text_
                [ SA.x <| String.fromFloat labelX
                , SA.y <| String.fromFloat labelY
                , SA.class "button__label"
                , SA.fill <| Color.toCssString <| Tone.text style state
                ]
                [ Svg.text label ]
    in case face of
        Default ->
            if CS.isHorizontal cellShape
                then case selected of
                    Selected ->
                        Svg.g
                            []
                            [ Svg.g
                                [ SA.style <|
                                    "transform: "
                                        ++ "translate(" ++ String.fromFloat Cell.gap ++ "px,"
                                                        ++ String.fromFloat (cy - 4) ++ "px)" ]
                                [ Util.arrow (Tone.text style state) (scale 0.5) (rotate 90)
                                ]
                            -- , textLabel ( bounds.width / 2.25 + gap, cy )
                            , textLabel ()
                            ]
                    Usual -> textLabel ()
                else textLabel ()
        WithIcon (Icon icon) ->
            let
                iconUrl =
                    "./assets/" ++ icon ++ "_" ++ Theme.toString theme ++ ".svg"
                ( iconWidth, iconHeight ) = ( bounds.width / 2.25, bounds.height / 2.25 )
                ( iconX, iconY ) =
                    if CS.isHorizontal cellShape
                        then
                            ( -23, cy - iconHeight / 2 + 1 )
                        else
                            ( cx - iconWidth / 2, cy - iconHeight / 2 + 1 )
            in
                Svg.g
                    [ ]
                    [
                        Svg.image
                        [ SA.xlinkHref <| iconUrl
                        , SA.class "button__icon"
                        , SA.width <| String.fromFloat iconWidth ++ "px"
                        , SA.height <| String.fromFloat iconHeight ++ "px"
                        , SA.x <| String.fromFloat iconX
                        , SA.y <| String.fromFloat iconY
                        ]
                        []
                    , if CS.isHorizontal cellShape
                        then textLabel ()
                        else Svg.none
                    ]
        WithColor theColor ->
            let
                ( rectWidth, rectHeight ) = ( bounds.width, bounds.height )
                ( rectX, rectY ) = ( cx - rectWidth / 2, cy - rectHeight / 2 )
            in
                Svg.rect
                    [ SA.x <| String.fromFloat rectX
                    , SA.y <| String.fromFloat rectY
                    , SA.width <| String.fromFloat rectWidth
                    , SA.height <| String.fromFloat rectHeight
                    , SA.fill <| Color.toCssString theColor
                    , SA.rx "3"
                    , SA.ry "3"
                    ]
                    [
                    ]


color : Style -> State -> Color -> Bounds -> Svg msg
color _ _ value bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        radius = (min bounds.width bounds.height) / 6
    in Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r <| String.fromFloat radius
        , SA.fill <| Color.toCssString value
        ]
        [
        ]


arrow : Style -> State -> NestState -> Bounds -> Svg msg
arrow style state groupState bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        scaleV = (min bounds.width bounds.height) / 127
    in Svg.g
        [ SA.style <|
            "transform: "
                ++ "translate(" ++ String.fromFloat (center.x - (14 * scaleV)) ++ "px,"
                                ++ String.fromFloat (center.y - (14 * scaleV)) ++ "px)"
        ]
        [ Util.arrow (Tone.lines style state) (scale scaleV)
            <| case groupState of
                Expanded -> rotate 180
                Detached -> rotate 45
                Collapsed -> rotate 0
        ]


makeClass : Tone -> CellShape -> Property msg -> String
makeClass tone shape prop =
    "cell"
        ++ " cell--" ++
            ( case prop of
                Nil -> "ghost"
                Number _ -> "number"
                Coordinate _ -> "coord"
                Text _ -> "text"
                Color _ -> "color"
                Toggle _ -> "toggle"
                Action _ -> "button"
                Choice _ _ _ -> "choice"
                Group _ _ _ -> "group"
            )
        ++ " cell--" ++ toneToModifier tone
        ++ " cell--" ++ shapeToModifier shape
