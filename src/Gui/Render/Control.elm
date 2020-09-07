module Gui.Render.Control exposing (..)

import Array
import Array exposing (Array)

import Html exposing (Html, text, div, span, input)
import Svg exposing (..)
import Svg.Attributes as S exposing (..)

import Gui.Control exposing (..)
import Gui.Over exposing (..)
import Gui.Msg exposing (..)


import BinPack exposing (Bounds)


textAttrs : Float -> Float -> String -> List (Attribute msg)
textAttrs xPos yPos color =
    [ fill color
    , x <| String.fromFloat xPos
    , y <| String.fromFloat yPos
    , fontSize "12"
    , S.style "font-family: sans-serif;"
    , textAnchor "middle"
    , alignmentBaseline "central"
    ]


circleAttrs : Float -> Float -> String -> List (Attribute msg)
circleAttrs xPos yPos color =
    [ cx <| String.fromFloat <| xPos
    , cy <| String.fromFloat <| yPos
    , r "15"
    , stroke color, fill "none"
    , strokeWidth lineWidth
    ]


knobRectAttrs : String -> Float -> List (Attribute msg)
knobRectAttrs color rotation =
    [ width "5"
    , height "5"
    , x <| String.fromFloat (-2.5)
    , y <| String.fromFloat (-2.5)
    , fill color
    , stroke "none"
    , transform
        <| "rotate(" ++ String.fromFloat rotation ++ ") translate(0,-15)"
    ]


xyRectAttrs : String -> List (Attribute msg)
xyRectAttrs color =
    [ width <| String.fromInt (cellWidth - 5)
    , height <| String.fromInt (cellHeight - 5)
    , x <| String.fromFloat 2.5
    , y <| String.fromFloat 2.5
    , rx <| String.fromFloat 10
    , ry <| String.fromFloat 10
    , fill color
    , stroke "none"
    ]


xySmallRectAttrs : Float -> Float -> String -> List (Attribute msg)
xySmallRectAttrs xPos yPos color =
    [ width "5"
    , height "5"
    , x <| String.fromFloat xPos
    , y <| String.fromFloat yPos
    , fill color
    , stroke "none"
    ]



upArrow : Float -> Float -> String -> Svg msg
upArrow xPos yPos color =
    g
        [ class "gui-arrow"
        , transform "rotate(-180.000000) translate(-30, -12)" ]
        [
            Svg.path
                [ x <| String.fromFloat <| xPos
                , y <| String.fromFloat <| yPos
                --transform "translate(-37.125,-290.25)"
                , d "m 0 0 l 20 20 l 20 -20"
                , stroke color, fill "none"
                , strokeWidth lineWidth
                , S.strokeLinecap "round" ]
                []
        ]


downArrow : Float -> Float -> String -> Svg msg
downArrow xPos yPos color =
    g
        [ class "gui-arrow"
        , transform "rotate(-180.000000) translate(-30, -12)" ]
        [
            Svg.path
                [ x <| String.fromFloat <| xPos
                , y <| String.fromFloat <| yPos
                --transform "translate(-37.125,-290.25)"
                , d "m 0 20 l 20 -20 l 20 20"
                , stroke color, fill "none"
                , strokeWidth lineWidth
                , S.strokeLinecap "round" ]
                []
        ]


knob : Control Axis Float msg -> Svg msg
knob (Control { min, max, step } value _) =
    let
        roundBy = 2
        default = 0
        friendlyValue =
            toFloat (round (value * toFloat roundBy)) / toFloat roundBy
        normalizedValue = (value - min) / (max - min)
        normalizedDefault = (default - min) / (max - min)
        rotationAngle = 360 * normalizedValue
        defaultRotationAngle = 360 * normalizedDefault
    in
        g [ class "gui-knob" ]
            [ text_
                (textAttrs (cellWidth / 2) (cellHeight / 3) baseColor)
                [ Svg.text <| String.fromFloat friendlyValue ]
            , g
                [ transform
                    <| "translate(" ++ String.fromFloat (cellWidth / 2) ++ ","
                    ++ String.fromFloat (cellHeight / 3) ++ ")"
                ]
                [ circle
                    (circleAttrs 0 0 baseColor)
                    []
                , rect
                    (knobRectAttrs "rgba(0,255,0,0.4)" defaultRotationAngle)
                    []
                , rect
                    (knobRectAttrs baseColor rotationAngle)
                    []
                ]
            ]


xy : Control ( Axis, Axis ) ( Float, Float ) msg -> Svg msg
xy ( Control ( xConf, yConf ) ( valueX, valueY ) _ ) =
    let
        friendlyValue value roundBy =
            toFloat (round (value * toFloat roundBy)) / toFloat roundBy
        normalizedValue { min, max } value = (value - min) / (max - min)
        normalizedDefault { min, max, default } = (default - min) / (max - min)
        normalizedX = normalizedValue xConf valueX
        normalizedY = normalizedValue yConf valueY
        ( xRelPos, yRelPos ) =
            ( normalizedX * (cellWidth - 5)
            , normalizedY * (cellHeight - 5)
            )
    in
        g [ class "gui-xy" ]
            [ rect
                (xyRectAttrs nothingColor)
                []
            , rect
                (xySmallRectAttrs xRelPos yRelPos "rgba(0,255,0,0.4)")
                []
            , text_
                (textAttrs
                    xRelPos
                    yRelPos
                    baseColor)
                [ Svg.text <| "("
                    ++ (String.fromFloat <| friendlyValue valueX 2)
                    ++ ","
                    ++ (String.fromFloat <| friendlyValue valueY 2)
                    ++ ")"
                ]
            ]


toggle : Control x ToggleState msg -> Svg msg
toggle (Control _ state _) =
    g [ class "gui-toggle" ]
        [ text_
            (textAttrs (cellWidth / 2) (cellHeight / 3) baseColor)
            [ Svg.text <| if state == TurnedOn then "on" else "off" ]
        , circle
            (circleAttrs (cellWidth / 2) (cellHeight / 3) <|
                if state == TurnedOn then onColor else offColor)
            []
        ]


button : Control a b msg -> Svg msg
button _ =
    g []
        [ circle
            (circleAttrs (cellWidth / 2) (cellHeight / 3) nothingColor)
            []
        ]


nest : Control ( Shape, Array ( Label, Over msg ) ) ( ExpandState, Maybe Focus ) msg -> Svg msg
nest (Control ( _, _ ) ( state, _ ) _) =
    g [ class "gui-nested" ]
        [ g
            [ transform
                <| "translate("
                    ++ (String.fromFloat <| cellWidth / 2 - 10) ++ ",10)" ]
            [ if state == Expanded
                then downArrow (cellWidth / 2) 10 baseColor
                else upArrow (cellWidth / 2) 10 baseColor  ]
        ]

{-
choice : Control ( Array ( Maybe Icon, Label ) ) Int msg -> Svg msg
choice (Control cells itemChosen _) =
    g [ class "gui-choice" ]
        [ text_
            (textAttrs (cellWidth / 2) (cellHeight / 2) baseColor)
            [ cells
                |> Array.fromList |> Array.get itemChosen
                |> Maybe.map cellLabel |> Maybe.withDefault "" |> Svg.text ]
        , g
            [ transform
                <| "translate("
                    ++ (String.fromFloat <| cellWidth / 2 - 10) ++ ",10)" ]
            [ if state == Expanded
                then downArrow (cellWidth / 2) 10 baseColor
                else upArrow (cellWidth / 2) 10 baseColor ]
        ]
-}





{-
renderCell : NestPos -> Focus -> Maybe SelectionState -> Cell umsg -> Html (Msg umsg)
renderCell position (Focus focus) isSelected cell =
    let cellBody =
            case cell of
                Ghost _ ->
                    g [ class "gui-ghost" ]
                        [ ]
                Choice _ state itemChosen _ { cells } ->
                    g [ class "gui-choice" ]
                        [ text_
                            (textAttrs (cellWidth / 2) (cellHeight / 2) baseColor)
                            [ cells
                                |> Array.fromList |> Array.get itemChosen
                                |> Maybe.map cellLabel |> Maybe.withDefault "" |> Svg.text ]
                        , g
                            [ transform
                                <| "translate("
                                    ++ (String.fromFloat <| cellWidth / 2 - 10) ++ ",10)" ]
                            [ if state == Expanded
                                then downArrow (cellWidth / 2) 10 baseColor
                                else upArrow (cellWidth / 2) 10 baseColor ]
                        ]
                Nested _ state { cells } ->
                    g [ class "gui-nested" ]
                        [ g
                            [ transform
                                <| "translate("
                                    ++ (String.fromFloat <| cellWidth / 2 - 10) ++ ",10)" ]
                            [ if state == Expanded
                                then downArrow (cellWidth / 2) 10 baseColor
                                else upArrow (cellWidth / 2) 10 baseColor  ]
                        ]
                ChoiceItem _ ->
                    g [ class "gui-choice-item" ]
                        [ g
                            [ transform
                                <| "translate("
                                    ++ (String.fromFloat <| cellWidth / 2 - 10) ++ ",10)" ]
                            [ case isSelected of
                                (Just Selected) -> downArrow (cellWidth / 2) 10 onColor
                                _ -> downArrow (cellWidth / 2) 10 nothingColor ]
                        ]
                Button _ _ ->
                    g []
                        [ circle
                            (circleAttrs (cellWidth / 2) (cellHeight / 3) nothingColor)
                            []
                        ]
        cellLabel labeledCell =
            case labeledCell of
                Ghost label -> label
                Knob label _ _ _ -> label
                XY label _ _ _ -> label
                Toggle label _ _ -> label
                Button label _ -> label
                Nested label _ _ -> label
                Choice label _ itemChosen _ _ -> label
                ChoiceItem label -> label
        labelBackWidth = cellWidth * 2/3
        labelBackHeight = 20
    in svg
        [ width <| String.fromInt cellWidth
        , height <| String.fromInt cellHeight
        ]
        [ cellBody
--        , rect
--            [ x <| toString <| (cellWidth / 2) - (labelBackWidth / 2)
--            , y <| toString <| cellHeight - labelBackHeight
--            , rx "5", ry "5"
--            , width <| toString <| labelBackWidth, height <| toString <| labelBackHeight
--            , fill "rgba(120,0,0,0.4)" ] []
        , text_
            (textAttrs (cellWidth / 2) (cellHeight - 10) "white")
            [ Svg.text <| cellLabel cell ]
        ]
-}
