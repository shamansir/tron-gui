module Tron.Render.Debug exposing (..)


import Bounds exposing (..)

import Svg exposing (Svg)
import Color
import Array

import Tron.Control exposing (Control(..))
import Tron.Tree.Internals exposing (..)
import Tron.Control.Impl.Toggle exposing (..)
import Tron.Control.Impl.Nest exposing (..)
import Tron.Render.Util exposing (..)
import Tron.Path as Path
import Tron.Pages as Pages


boundsDebug : BoundsF -> Svg a
boundsDebug b =
    Svg.g []
        [ textAt 5 5 <| "(" ++ String.fromFloat b.x ++ "," ++ String.fromFloat b.y ++ ")"
        , textAt 40 5 <| String.fromFloat b.width ++ "x" ++ String.fromFloat b.height
        ]


propertyDebug : ( Path.Label, Tree a ) -> Svg a
propertyDebug ( label, prop )  =
    case prop of
        Nil _ ->
            Svg.g []
                [ textAt 5 5 <| label ++ " ghost" ]
        Number (Control { min, step, max } ( maybeFrom, val ) _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " knob: "
                , textAt 5 20
                    <| String.fromFloat min ++ "/"
                    ++ String.fromFloat step ++ "/"
                    ++ String.fromFloat max
                    ++ " " ++ String.fromFloat val
                    ++ " " ++ (maybeFrom |> Maybe.map String.fromFloat |> Maybe.withDefault "-")
                ]
        Coordinate (Control ( xConf, yConf ) ( _, ( valX, valY ) ) _) ->
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
        Color (Control _ ( _, color ) _) ->
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
        {- Switch (Control items ( _, value ) _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " switch: " ++ String.fromInt value
                    ++ (items |> Array.get value |> Maybe.withDefault "-")
                ] -}
        Group maybeFocus _ (Control _ { form } _) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " nested: "
                , textAt 5 20
                    <| if form == Expanded then "expanded" else "collapsed"
                , textAt 5 35
                    <| "focus: " ++
                    case maybeFocus of
                        Just (FocusAt focus) -> String.fromInt focus
                        _ -> "none"
                ]
        Choice maybeFocus _ (Control _ state _ ) ->
            Svg.g []
                [ textAt 5 5 <| label ++ " choice: "
                , textAt 5 20
                    <| if state.form == Expanded then "expanded" else "collapsed"
                , textAt 5 35
                    <| "focus: " ++
                    case maybeFocus of
                        Just (FocusAt focus) -> String.fromInt focus
                        _ -> "none"
                , textAt 5 50
                    <| " selected: " ++ String.fromInt (Pages.numifyItem state.selected)
                ]

        Live innerProp ->
            propertyDebug ( label, innerProp )
