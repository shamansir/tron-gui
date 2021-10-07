module Tron.Render.Property exposing (..)


import Bounds exposing (..)

import Array exposing (Array)
import Svg exposing (Svg)
import Svg.Attributes as SA
import Html
import Html.Attributes as HA
import Html.Events as HE

import Axis exposing (Axis)

import Tron.Property exposing (..)
import Tron.Property.Controls exposing (..)
import Tron.Path as Path exposing (Path)
import Tron.Msg exposing (Msg_(..))
import Tron.Focus exposing (Focused(..))
import Tron.Control as Core exposing (Control(..))

import Tron.Control.Text exposing (TextState(..))
import Tron.Control.Button exposing (Face(..), Icon(..), urlToString)
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
import Url


view
     : Theme
    -> State
    -> Path
    -> BoundsF
    -> Maybe ( Path.Label, Property a )
    -> CellShape
    -> ( Path.Label, Property a )
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
            , SA.strokeWidth
                <| String.fromInt (strokeWidthFor state <| isExpanded prop) ++ "px"
            , SA.stroke
                <| Color.toCssString
                <| Coloring.border theme state <| isExpanded prop
            --, SA.strokeDasharray <| strokeDashFor state
            ]
            []
        , viewProperty
            theme state path bounds maybeSelectedInside cellShape ( label, prop )
        , {- case prop of
            Action _ -> Svg.none
            _ -> -} viewLabel theme state cellShape bounds label
        ]


viewLabel
    :  Theme
    -> State
    -> CellShape
    -> BoundsF
    -> Path.Label
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
    -> BoundsF
    -> Maybe ( Path.Label, Property a )
    -> CellShape
    -> ( Path.Label, Property a )
    -> Svg Msg_
viewProperty
    theme
    ( ( placement, focus, _ ) as state )
    path
    bounds
    maybeSelectedInside
    cellShape
    ( label, prop ) =
    case prop of

        Number (Control { min, max } ( _, value ) _) ->

            knob
                theme
                state
                bounds
                (if value < min then min
                   else if value > max then max
                   else value)
                (if value < min then 0
                   else if value > max then 1
                   else (value - min) / (max - min))

        Coordinate (Control ( xAxis, yAxis ) ( _, ( xValue, yValue ) ) _) ->

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

        {- Switch (Control items ( _, value ) _) ->

            switch theme state bounds items value -}

        Color (Control _ (_, value) _) ->

            color theme state value bounds

        Choice _ _ ( Control items { form, face, mode, selected } _) ->

            case ( mode, face, maybeSelectedInside ) of

                ( _, Just buttonFace, _ ) ->
                    button theme state buttonFace cellShape label bounds

                ( Nest.Pages, Nothing, Just theSelectedProp ) ->
                    viewProperty
                        theme
                        ( placement, focus, Selected )
                        path
                        bounds
                        Nothing
                        cellShape
                        theSelectedProp

                ( Nest.SwitchThrough, Nothing, Just theSelectedProp ) ->
                    viewProperty
                        theme
                        ( placement, focus, Selected )
                        path
                        bounds
                        Nothing
                        cellShape
                        theSelectedProp

                ( Nest.Knob, Nothing, _ ) ->
                    knobSwitch
                        theme
                        state
                        bounds
                        (items |> Array.map Tuple.first)
                        selected

                ( Nest.SwitchThrough, Nothing, Nothing ) ->
                    arrow theme state form bounds

                ( Nest.Pages, Nothing, Nothing ) ->
                    arrow theme state form bounds

        Group _ _ ( Control _ { form, face } _) ->

            case face of
                Just buttonFace ->

                    button theme state buttonFace cellShape label bounds

                Nothing -> arrow theme state form bounds

        Live innerProp ->
            viewProperty
                theme
                state
                path
                bounds
                maybeSelectedInside
                cellShape
                ( label, innerProp )

        _ -> Svg.none


knob : Theme -> State -> BoundsF -> Float -> Float -> Svg msg
knob theme state bounds value relValue =
    let
        toAngle v = (-120) + (v * 120 * 2)
        minAngle = toAngle 0
        maxAngle =toAngle 1
        -- FIXME: move aligning the value to control
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
        roundedValue = Basics.toFloat (floor (value * 100)) / 100
        valueAngle =
            if relValue > 1 then maxAngle else
            if relValue < 0 then minAngle
            else toAngle relValue
    in
        Svg.g
            [ resetTransform ]
            [ path (Coloring.lines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = minAngle
                    , to = valueAngle
                    }
            , path (Coloring.secondaryLines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = valueAngle
                    , to = maxAngle
                    }
            , path (Coloring.lines theme state |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    valueAngle
            , Svg.text_
                [ SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat cy
                , SA.class "knob__value"
                , SA.style "pointer-events: none"
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                ]
                [ Svg.text <| String.fromFloat roundedValue ]
            ]


knobSwitch : Theme -> State -> BoundsF -> Array String -> Int -> Svg msg
knobSwitch theme state bounds items curItemId =
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
        curItem = items |> Array.get curItemId |> Maybe.withDefault "?"
        relValue = Basics.toFloat curItemId / Basics.toFloat (Array.length items)
    in
        Svg.g
            [ resetTransform ]
            [ path (Coloring.lines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle 0, to = toAngle relValue }
            , path (Coloring.secondaryLines theme state |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle relValue, to = toAngle 1 }
            , path (Coloring.lines theme state |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    (toAngle relValue)
            , Svg.text_
                [ SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat cy
                , SA.class "knob__value"
                , SA.style "pointer-events: none"
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                ]
                [ Svg.text curItem ]
            ]


coord : Theme -> State -> BoundsF -> ( Float, Float ) -> Svg msg
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
    -> BoundsF
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
                --, SA.mask "url(#button-text-mask)"
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


toggle : Theme -> State -> ToggleState -> BoundsF -> Svg msg
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



button : Theme -> State -> Face -> CellShape -> Path.Label -> BoundsF -> Svg msg
button theme ( ( _, _, selected ) as state ) face cellShape label bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        ( labelX, labelY ) =
            if CS.isHorizontal cellShape
                then
                    case face of
                        Default -> ( 30, cy + 4 )
                        WithIcon _ -> ( 40, cy )
                        WithColor _ -> ( 40, cy )
                else ( cx, cy )
        textLabel _ =
            Svg.text_
                [ SA.x <| String.fromFloat labelX
                , SA.y <| String.fromFloat labelY
                , SA.class "button__label"
                , SA.fill <| Color.toCssString <| Coloring.text theme state
                , SA.mask <|
                    if not <| CS.isHorizontal cellShape
                        then "url(#button-text-mask)"
                        else "url(#button-text-mask-wide)"
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
                    icon theme |> urlToString
                    --"./assets/" ++ icon ++ "_" ++ Theme.toString theme ++ ".svg"
                ( iconWidth, iconHeight ) = iconSize cellShape bounds
                ( iconX, iconY ) =
                    if CS.isHorizontal cellShape
                        then
                            ( -20, cy - iconHeight / 2 + 1 )
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
            case CS.units cellShape of
                ( CS.Single, CS.Single ) ->
                    color theme state theColor bounds
                ( CS.Twice, _ ) ->
                    Svg.g
                        []
                        [ color theme state theColor
                            <| { bounds
                               | width = bounds.height
                               }
                        , textLabel ()
                        ]
                _ ->
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


color : Theme -> State -> Color -> BoundsF -> Svg msg
color theme state value bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        radius = (min bounds.width bounds.height) / 6
    in Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r <| String.fromFloat radius
        , SA.fill <| Color.toCssString value
        , SA.strokeWidth "1"
        , SA.stroke <| Color.toCssString <| Coloring.lines theme state
        ]
        [
        ]


arrow : Theme -> State -> Form -> BoundsF -> Svg msg
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


makeClass : CellShape -> Property a -> String
makeClass shape prop =
    let
        propTypeId prop_ =
            case prop_ of
                Nil _ -> "ghost"
                Number _ -> "number"
                Coordinate _ -> "coord"
                Text _ -> "text"
                Color _ -> "color"
                Toggle _ -> "toggle"
                Action _ -> "button"
                Choice _ _ _ -> "choice"
                Group _ _ _ -> "group"
                Live innerProp -> propTypeId innerProp
    in
    "cell"
        ++ " cell--" ++ propTypeId prop
        ++ " cell--" ++ shapeToModifier shape


iconSize : CellShape -> BoundsF -> ( Float, Float )
iconSize cs bounds =
    case CS.units cs of
        ( CS.Single, CS.Single ) -> ( 32, 32 )
        _ -> ( bounds.width / 2.25, bounds.height / 2.25 )


strokeWidthFor : State -> Maybe Nest.Form -> Int
strokeWidthFor ( placement, focused, selected ) maybeCollapsed =
    case maybeCollapsed of
        Just Nest.Expanded -> 1
        _ ->
            case selected of
                Selected -> 1
                Usual ->
                    case focused of
                        FocusedBy _  -> 1
                        _ -> 0


strokeDashFor : State -> String
strokeDashFor ( _, focused , _ ) =
    case focused of
        FocusedBy _ -> "5,1"
        NotFocused -> ""