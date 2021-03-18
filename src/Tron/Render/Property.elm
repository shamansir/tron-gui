module Tron.Render.Property exposing (..)



import BinPack exposing (Bounds)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html
import Html.Attributes as HA
import Html.Events as HE

import Axis exposing (Axis)

import Tron.Property exposing (..)
import Tron.Path as Path exposing (Path)
import Tron.Msg exposing (Msg_(..))
import Tron.Focus exposing (Focused(..))
import Tron.Control as Core exposing (Control(..))

import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button exposing (Face(..), Icon(..))
import Tron.Control.Toggle exposing (ToggleState(..))
import Tron.Control.Nest as Nest exposing (getForm, Form(..))
import Tron.Control.Number as Number exposing (Control)

import Tron.Render.Transform exposing (..)
import Tron.Render.Util exposing (..)
import Tron.Render.Util as Svg exposing (none)
import Tron.Render.Util as Util exposing (arrow)

import Tron.Style.Logic exposing (..)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS exposing (..)

import Tron.Style.Coloring as Coloring exposing (..)
import Tron.Style.Theme exposing (Theme(..))
import Tron.Style.Theme as Theme exposing (toString)
import Tron.Style.Placement exposing (Placement)
import Tron.Style.Selected exposing (Selected(..))
import Tron.Style.Cell as Cell

import Color as Color exposing (..)


view
     : Theme
    -> State
    -> Path
    -> Bounds
    -> Maybe ( Label, Property msg )
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg_
view theme state path bounds maybeSelectedInside cellShape ( label, prop ) =
    Svg.g
        [ HE.onClick <| Click path
        , SA.class <| makeClass cellShape <| prop
        ]
        [ Svg.rect
            [ SA.fill
                <| Color.toCssString
                <| Coloring.back theme state
            , SA.x <| String.fromFloat (Cell.gap / 2)
            , SA.y <| String.fromFloat (Cell.gap / 2)
            , SA.rx <| String.fromFloat Cell.borderRadius
            , SA.ry <| String.fromFloat Cell.borderRadius
            , SA.width <| String.fromFloat (bounds.width - Cell.gap) ++ "px"
            , SA.height <| String.fromFloat (bounds.height - Cell.gap) ++ "px"
            ]
            []
        , viewProperty
            theme state path bounds maybeSelectedInside cellShape ( label, prop )
        , case prop of
            Action _ -> Svg.none
            _ -> viewLabel theme state cellShape bounds label
        ]


viewLabel
    :  Theme
    -> State
    -> CellShape
    -> Bounds
    -> Label
    -> Svg msg
viewLabel theme state cellShape bounds label =
    if CS.isSquare cellShape
        then
            Svg.text_
                [ SA.class "cell__label"
                , SA.x <| String.fromFloat (bounds.width / 2)
                , SA.y <| String.fromFloat (bounds.height / 5 * 4)
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                ]
                [ Svg.text label ]
        else Svg.none


viewProperty
     : Theme
    -> State
    -> Path
    -> Bounds
    -> Maybe ( Label, Property msg )
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg_
viewProperty
    theme
    ( ( placement, focus, selected ) as state )
    path
    bounds
    maybeSelectedInside
    cellShape
    ( label, prop ) =
    case prop of

        Number (Control { min, max } value _) ->

            knob
                theme
                state
                bounds
                <| (value - min) / (max - min)

        Coordinate (Control ( xAxis, yAxis ) ( xValue, yValue ) _) ->

            coord
                theme
                state
                bounds
                <|
                    ( (xValue - xAxis.min) / (xAxis.max - xAxis.min)
                    , (yValue - yAxis.min) / (yAxis.max - yAxis.min)
                    )

        Text (Control _ value _) ->

            text theme state value (TextInput path) bounds

        Toggle (Control _ value _) ->

            toggle theme state value bounds

        Action (Control face _ _) ->

            button theme state face cellShape label bounds

        Color (Control _ value _) ->

            color theme state value bounds

        Choice _ _ ( Control _ { form } _) ->

            case maybeSelectedInside of
                Just theSelectedProp ->
                    viewProperty
                        theme
                        ( placement, focus, Selected )
                        path
                        bounds
                        Nothing
                        cellShape
                        theSelectedProp
                Nothing ->
                    arrow theme state form bounds

        Group _ _ ( Control _ { form } _) ->

            arrow theme state form bounds

        _ -> Svg.none


knob : Theme -> State -> Bounds -> Float -> Svg msg
knob theme state bounds value =
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
            [ resetTransform ]
            [ path (Coloring.lines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle 0, to = toAngle value }
            , path (Coloring.secondaryLines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle value, to = toAngle 1 }
            , path (Coloring.lines theme state |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    (toAngle value)
            ]


coord : Theme -> State -> Bounds -> ( Float, Float ) -> Svg msg
coord theme state bounds ( valueX, valueY ) =
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
        [ resetTransform ]
        [ Svg.line
            [ SA.x1 <| String.fromFloat left
            , SA.y1 <| String.fromFloat cy
            , SA.x2 <| String.fromFloat right
            , SA.y2 <| String.fromFloat cy
            , SA.stroke <| Color.toCssString <| Coloring.lines theme state
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
            , SA.stroke <| Color.toCssString <| Coloring.lines theme state
            , SA.opacity "0.2"
            , SA.strokeWidth "1"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ SA.cx <| String.fromFloat circleX
            , SA.cy <| String.fromFloat circleY
            , SA.fill <| Color.toCssString <| Coloring.lines theme state
            , SA.fill "none"
            , SA.stroke <| Color.toCssString <| Coloring.lines theme state
            , SA.strokeWidth "2"
            , SA.r <| String.fromFloat circleRadius
            ]
            []
        ]
-- 5 30 10 30 10 30 10 30 5


text
    :  Theme
    -> State
    -> ( TextState, String )
    -> (String -> msg)
    -> Bounds
    -> Svg msg
text theme state ( editing, value ) onInput bounds =
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
                , SA.fill <| Color.toCssString <| Coloring.lines theme state
                ]
                [ Svg.text <|
                    if String.length value <= 6 then
                        value
                    else (value |> String.left 6) ++ ".." ]
        Editing ->
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
                        , HE.onInput onInput
                        , HA.value value
                        ]
                        [ ]
                    ]
                ]


toggle : Theme -> State -> ToggleState -> Bounds -> Svg msg
toggle theme _ tstate bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) )
        ( rectX, rectY ) = ( cx - 36 / 2, cy - 20 / 2 )
        circleRadius = 8
        rectCornerRadius = 10
        ( rectWidth, rectHeight ) = ( 36, 20 )
        ( circleX, circleY ) =
            ( case tstate of
                TurnedOff -> rectX + 2 + circleRadius
                TurnedOn -> rectX + rectWidth - 2 - circleRadius
            , cy
            )
    in
        Svg.g
            []
            [ Svg.rect
                [ SA.x <| String.fromFloat rectX
                , SA.y <| String.fromFloat rectY
                , SA.rx <| String.fromFloat rectCornerRadius
                , SA.ry <| String.fromFloat rectCornerRadius
                , SA.width <| String.fromFloat rectWidth
                , SA.height <| String.fromFloat rectHeight
                , SA.fill <| case (theme, tstate) of
                    (Theme.Dark, TurnedOn) -> Color.toCssString Color.white
                    (Theme.Dark, TurnedOff) -> Color.toCssString <| Color.rgba 1 1 1 0.4
                    (Theme.Light, TurnedOn) -> Color.toCssString Color.black
                    (Theme.Light, TurnedOff) -> Color.toCssString <| Color.rgba 0 0 0 0.2
                ]
                [ ]
            , Svg.circle
                [ SA.cx <| String.fromFloat circleX
                , SA.cy <| String.fromFloat circleY
                , SA.r <| String.fromFloat 8
                , SA.fill <| case theme of
                  Theme.Dark -> Color.toCssString Color.black
                  Theme.Light -> Color.toCssString Color.white
                ]
                []
            ]



button : Theme -> State -> Face -> CellShape -> Label -> Bounds -> Svg msg
button theme ( ( _, _, selected ) as state ) face cellShape label bounds =
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
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                ]
                [ Svg.text label ]
    in case face of
        Default ->
            if CS.isHorizontal cellShape
                then case selected of
                    Selected ->
                        Svg.g
                            [ resetTransform ]
                            [ Svg.g
                                [ SA.style <|
                                    "transform: "
                                        ++ "translate(" ++ String.fromFloat Cell.gap ++ "px,"
                                                        ++ String.fromFloat (cy - 4) ++ "px)" ]
                                [ Util.arrow (Coloring.text theme state) (scale 0.5) (rotate 90)
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
                    [ resetTransform ]
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


color : Theme -> State -> Color -> Bounds -> Svg msg
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


arrow : Theme -> State -> Form -> Bounds -> Svg msg
arrow theme state form bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        scaleV = (min bounds.width bounds.height) / 127
    in Svg.g
        [ SA.style <|
            "transform: "
                ++ "translate(" ++ String.fromFloat (center.x - (14 * scaleV)) ++ "px,"
                                ++ String.fromFloat (center.y - (14 * scaleV)) ++ "px)"
        ]
        [ Util.arrow (Coloring.lines theme state) (scale scaleV)
            <| case form of
                Expanded -> rotate 180
                Detached -> rotate 45
                Collapsed -> rotate 0
        ]


makeClass : CellShape -> Property msg -> String
makeClass shape prop =
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
        ++ " cell--" ++ shapeToModifier shape
