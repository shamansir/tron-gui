module Gui.Render.Debug exposing (..)


import Bounds exposing (..)

import Svg exposing (Svg)
import Color

import Gui.Control exposing (Control(..))
import Gui.Property exposing (..)

import Gui.Render.Util exposing (..)


boundsDebug : Bounds -> Svg msg
boundsDebug b =
    Svg.g []
        [ textAt 5 5 <| "(" ++ String.fromFloat b.x ++ "," ++ String.fromFloat b.y ++ ")"
        , textAt 40 5 <| String.fromFloat b.width ++ "x" ++ String.fromFloat b.height
        ]


propertyDebug : ( Label, Property msg ) -> Svg msg
propertyDebug ( label, prop )  =
    case prop of
        Nil ->
            Svg.g []
                [ textAt 5 5 <| label ++ " ghost" ]
        Number (Control { min, step, max } val _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " knob: "
                , textAt 5 20
                    <| String.fromFloat min ++ "/"
                    ++ String.fromFloat step ++ "/"
                    ++ String.fromFloat max
                    ++ " " ++ String.fromFloat val ]
        Coordinate (Control ( xConf, yConf ) ( valX, valY ) _) ->
            Svg.g []
                [ textAt 5 5 <| "xy: " ++ label
                , textAt 5 20
                    <| String.fromFloat xConf.min ++ "/"
                    ++ String.fromFloat xConf.step ++ "/"
                    ++ String.fromFloat xConf.max
                    ++ " " ++ String.fromFloat valX
                    ++ " " ++ String.fromFloat valY
                    ++ " " ++ String.fromFloat yConf.min ++ "/"
                    ++ String.fromFloat yConf.step ++ "/"
                    ++ String.fromFloat yConf.max ]
        Toggle (Control _ val _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " toggle: "
                , textAt 5 20
                    <| if val == TurnedOn then "on" else "off"
                ]
        Color (Control _ color _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " color: " ++ Color.toCssString color
                ]
        Text (Control _ ( _, value ) _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " text: " ++ value
                ]
        Action _ ->
            Svg.g []
                [ textAt 5 5 <| label ++ " button" ]
        Group (Control _ ( state, maybeFocus ) _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " nested: "
                , textAt 5 20
                    <| if state == Expanded then "expanded" else "collapsed"
                , textAt 5 35
                    <| "focus: " ++
                    case maybeFocus of
                        Just (FocusAt focus) -> String.fromInt focus
                        _ -> "none"
                ]
        Choice (Control _ ( state, ( maybeFocus, SelectedAt selected ) ) _ ) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " choice: "
                , textAt 5 20
                    <| if state == Expanded then "expanded" else "collapsed"
                , textAt 5 35
                    <| "focus: " ++
                    case maybeFocus of
                        Just (FocusAt focus) -> String.fromInt focus
                        _ -> "none"
                , textAt 5 50
                    <| " selected: " ++ String.fromInt selected
                ]
