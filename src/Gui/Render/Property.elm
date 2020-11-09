module Gui.Render.Property exposing (..)


import Color exposing (Color)

import Gui.Property exposing (..)

import Svg exposing (Svg)
import Svg.Attributes as SA
import Html
import Html.Attributes as HA
import Html.Events as HE

import Bounds exposing (Bounds)
import Gui.Path as Path exposing (Path)
import Gui.Msg exposing (Msg(..))
import Gui.Focus exposing (Focused(..))
import Gui.Control exposing (Control(..))
import Gui.Render.Util exposing (..)
import Gui.Render.Util as Svg exposing (none)
import Gui.Render.Util as Util exposing (arrow)
import Gui.Render.Style exposing (..)
import Gui.Render.StyleLogic exposing (..)
import Gui.Render.StyleLogic as Style exposing (text)


type Placement
    = AtRoot
    | OnAPlate


view
     : Placement
    -> Theme
    -> Tone
    -> Path
    -> Bounds
    -> Focused
    -> CellShape
    -> ( Label, Property msg )
    -> Svg Msg
view placement theme tone path bounds focus cellShape ( label, prop ) =
    Svg.g
        [ HE.onClick <| Click path
        , HA.style "pointer-events" "all"
        , HA.style "cursor" "pointer"
        , SA.class <| makeClass prop
        ]
        [
            Svg.rect
                [ SA.fill
                    <| Color.toCssString
                    <| case focusColor theme focus of
                        Just focusColor -> focusColor
                        Nothing ->
                            case placement of
                                AtRoot -> background theme
                                OnAPlate -> transparent
                , SA.x <| String.fromFloat (gap / 2)
                , SA.y <| String.fromFloat (gap / 2)
                , SA.rx <| String.fromFloat borderRadius
                , SA.ry <| String.fromFloat borderRadius
                , SA.width <| String.fromFloat (bounds.width - gap) ++ "px"
                , SA.height <| String.fromFloat (bounds.height - gap) ++ "px"
                ]
                []
        , case prop of
            Number (Control { min, max } value _) ->
                knob
                    theme
                    tone
                    bounds
                    <| (value - min) / (max - min)
            Coordinate (Control ( xAxis, yAxis ) ( xValue, yValue ) _) ->
                coord
                    theme
                    tone
                    bounds
                    <|
                        ( (xValue - xAxis.min) / (xAxis.max - xAxis.min)
                        , (yValue - yAxis.min) / (yAxis.max - yAxis.min)
                        )
            Text (Control _ value _) ->
                text theme tone value (TextInput path) bounds
            Toggle (Control _ value _) ->
                toggle theme tone value bounds
            Action (Control maybeIcon _ _) ->
                button theme tone maybeIcon cellShape label bounds
            Color (Control _ value _) ->
                color theme tone value bounds
            Choice (Control _ ( expanded, _ ) _) ->
                arrow theme tone expanded bounds
            Group (Control _ ( expanded, _ ) _) ->
                arrow theme tone expanded bounds
            _ -> Svg.none
        , case cellShape of
            Full ->
                Svg.text_
                    [ SA.textAnchor "middle"
                    , SA.dominantBaseline "middle"
                    , SA.x <| String.fromFloat (bounds.width / 2)
                    , SA.y <| String.fromFloat (bounds.height / 5 * 4)
                    , SA.fontSize "13px"
                    , SA.fontFamily fontFamily
                    , SA.fill <| Color.toCssString <| Style.text theme
                    ]
                    [ Svg.text label ]
            _ -> Svg.none
        ]


knob : Theme -> Tone -> Bounds -> Float -> Svg msg
knob theme tone bounds value =
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
            [ path (colorFor theme tone |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle 0, to = toAngle value }
            , path (knobLine theme |> Color.toCssString)
                <| describeArc
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    { from = toAngle value, to = toAngle 1 }
            , path (colorFor theme tone |> Color.toCssString)
                <| describeMark
                    { x = cx, y = cy }
                    { radiusA = radiusA, radiusB = radiusB }
                    (toAngle value)
            ]


coord : Theme -> Tone -> Bounds -> ( Float, Float ) -> Svg msg
coord theme tone bounds ( valueX, valueY ) =
    let
        ( cx, cy ) = ( bounds.width / 2, bounds.height / 2 )
        ( left, top ) = ( gap / 2, gap / 2 )
        ( right, bottom ) =
            ( bounds.width - gap / 2, bounds.height - gap / 2 )
        circleRadius = (max bounds.width bounds.height) / 18
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
            , SA.stroke <| Color.toCssString <| colorFor theme tone
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
            , SA.stroke <| Color.toCssString <| colorFor theme tone
            , SA.opacity "0.2"
            , SA.strokeWidth "1"
            , SA.strokeLinecap "round"
            ]
            []
        , Svg.circle
            [ SA.cx <| String.fromFloat circleX
            , SA.cy <| String.fromFloat circleY
            , SA.fill <| Color.toCssString <| colorFor theme tone
            , SA.fill "none"
            , SA.stroke <| Color.toCssString <| colorFor theme tone
            , SA.strokeWidth "2"
            , SA.r <| String.fromFloat circleRadius
            ]
            []
        ]
-- 5 30 10 30 10 30 10 30 5


text
    :  Theme
    -> Tone
    -> ( TextState, String )
    -> (String -> msg)
    -> Bounds
    -> Svg msg
text theme tone ( editing, value ) onInput bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        fontSize = bounds.width / 6
        lineHeight = fontSize * 1.6
        topShift = cy - lineHeight / 2
        maxWidth = bounds.width - gap * 2
    in case editing of
        Ready ->
            Svg.text_
                [ SA.dominantBaseline "middle"
                , SA.textAnchor "middle"
                , SA.fontSize <| String.fromFloat fontSize ++ "px"
                , SA.fontFamily fontFamily
                , SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat <| cy + 1
                ]
                [ Svg.text <|
                    if String.length value <= 6 then
                        value
                    else (value |> String.left 6) ++ ".." ]
        Editing ->
            Svg.foreignObject
                [ HA.style "width" <| String.fromFloat bounds.width  ++ "px"
                , HA.style "height" <| String.fromFloat bounds.height  ++ "px"
                ]
                [ Html.input
                    [ HA.style "max-width" <| String.fromFloat maxWidth ++ "px"
                    , HA.style "height" <| String.fromFloat lineHeight ++ "px"
                    , HA.style "position" "relative"
                    , HA.style "left" <| String.fromFloat gap ++ "px"
                    , HA.style "top" <| String.fromFloat topShift ++ "px"
                    , HA.style "outline" "none"
                    , HA.style "border" "none"
                    , HA.style "background-color" "transparent"
                    , HA.style "font-family" "IBM Plex Sans"
                    , HA.style "font-size" <| String.fromFloat fontSize ++ "px"
                    , HA.style "text-align" "center"
                    , HA.type_ "text"
                    , HA.placeholder "input"
                    , HE.onInput onInput
                    , HA.value value
                    ]
                    [ ]
                ]


toggle : Theme -> Tone -> ToggleState -> Bounds -> Svg msg
toggle theme tone state bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        radius = bounds.width / 7
    in Svg.circle
        [ SA.cx <| String.fromFloat cx
        , SA.cy <| String.fromFloat cy
        , SA.r <| String.fromFloat radius
        , SA.fill <| case state of
            TurnedOn -> Color.toCssString <| colorFor theme tone
            TurnedOff -> "none"
        , SA.stroke <| Color.toCssString <| colorFor theme tone
        , SA.strokeWidth "2"
        ]
        [
        ]


button : Theme -> Tone -> Maybe Icon -> CellShape -> Label -> Bounds -> Svg msg
button theme tone maybeIcon cellShape label bounds =
    let
        ( cx, cy ) = ( bounds.width / 2, (bounds.height / 2) - 3 )
        textLabel _ =
            Svg.text_
                [ SA.x <| String.fromFloat cx
                , SA.y <| String.fromFloat cy
                , HA.style "text-anchor" "middle"
                , HA.style "dominant-baseline" "central"
                , HA.style "font-family" fontFamily
                ]
                [ Svg.text label ]
    in case maybeIcon of
        Just (Icon icon) ->
            let
                postfix =
                    case theme of
                        Dark -> "dark"
                        Light -> "light"
                iconUrl =
                    "./assets/" ++ icon ++ "_" ++ postfix ++ ".svg"
                ( iconWidth, iconHeight ) = ( bounds.width / 2.25, bounds.height / 2.25 )
                ( iconX, iconY ) =
                    case cellShape of
                        TwiceByHalf ->
                            ( -1 * gap, cy - iconHeight / 2)
                        _ ->
                            ( cx - iconWidth / 2, cy - iconHeight / 2 )
            in
                Svg.g
                    [ ]
                    [
                        Svg.image
                        [ SA.xlinkHref <| iconUrl
                        , SA.class "button--icon"
                        , SA.width <| String.fromFloat iconWidth ++ "px"
                        , SA.height <| String.fromFloat iconHeight ++ "px"
                        , SA.x <| String.fromFloat iconX
                        , SA.y <| String.fromFloat iconY
                        ]
                        []
                    , case cellShape of
                        TwiceByHalf ->
                            textLabel ()
                        _ -> Svg.none
                    ]
        Nothing ->
            case cellShape of
                TwiceByHalf ->
                    textLabel ()
                _ ->
                    let
                        ( rectWidth, rectHeight ) = ( bounds.width / 4.5, bounds.height / 4.5 )
                        ( rectX, rectY ) = ( cx - rectWidth / 2, cy - rectHeight / 2 )
                    in
                        Svg.rect
                        [ SA.x <| String.fromFloat rectX
                        , SA.y <| String.fromFloat rectY
                        , SA.width <| String.fromFloat rectWidth
                        , SA.height <| String.fromFloat rectHeight
                        , SA.fill "none"
                        , SA.stroke <| Color.toCssString <| colorFor theme tone
                        , SA.strokeWidth "2"
                        , SA.strokeLinecap "round"
                        , SA.rx "3"
                        , SA.ry "3"
                        ]
                        [
                        ]


color : Theme -> Tone -> Color -> Bounds -> Svg msg
color theme tone value bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        radius = bounds.width / 6
    in Svg.circle
        [ SA.cx <| String.fromFloat center.x
        , SA.cy <| String.fromFloat center.y
        , SA.r <| String.fromFloat radius
        , SA.fill <| Color.toCssString value
        ]
        [
        ]


arrow : Theme -> Tone -> GroupState -> Bounds-> Svg msg
arrow theme tone groupState bounds =
    let
        center = { x = bounds.width / 2, y = (bounds.height / 2) - 3 }
        scaleV = bounds.width / 127
    in Svg.g
        [ SA.style <|
            "transform: "
                ++ "translate(" ++ String.fromFloat (center.x - (14 * scaleV)) ++ "px,"
                                ++ String.fromFloat (center.y - (14 * scaleV)) ++ "px)"
        ]
        [ Util.arrow (colorFor theme tone) (scale scaleV)
            <| case groupState of
                Expanded -> rotate 180
                Detached -> rotate 45
                Collapsed -> rotate 0
        ]


makeClass : Property msg -> String
makeClass prop =
    "cell " ++ case prop of
        Nil -> "ghost"
        Number _ -> "number"
        Coordinate _ -> "coord"
        Text _ -> "text"
        Color _ -> "color"
        Toggle _ -> "toggle"
        Action _ -> "button"
        Choice _ -> "choice"
        Group _ -> "group"
