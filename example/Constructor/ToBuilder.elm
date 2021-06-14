module Constructor.ToBuilder exposing (..)


import Array
import Color

import Tron exposing (Tron)

import Tron.Property exposing (..)
import Tron.Control exposing (..)
import Tron.Control.Value as V


toCodeLines : Tron () -> List String
toCodeLines root =
    [ "module Gui exposing (..)"
    , ""
    , "root : Tron ()"
    ] ++ "root = " :: (propToLines root |> indent 4)


propToLines : Tron () -> List String
propToLines prop =
    case prop of

        Nil -> []

        Number (Control { min, max, step } ( _, value ) _) ->
            [
                "Tron.number"
                , "    { min = " ++ String.fromFloat min
                , "    , max = " ++ String.fromFloat max
                , "    , step = " ++ String.fromFloat step
                , "    }"
                , "    " ++ String.fromFloat value
            ]

        Coordinate (Control (axisX, axisY) ( _, (valueX, valueY) ) _) ->
            [
                "Tron.xy"
                , "    ("
                , "        { min = " ++ String.fromFloat axisX.min
                , "        , max = " ++ String.fromFloat axisX.max
                , "        , step = " ++ String.fromFloat axisX.step
                , "        }"
                , "    ,"
                , "        { min = " ++ String.fromFloat axisY.min
                , "        , max = " ++ String.fromFloat axisY.max
                , "        , step = " ++ String.fromFloat axisY.step
                , "        }"
                , "    )"
                , "    ( " ++ String.fromFloat valueX ++ ", " ++ String.fromFloat valueY ++ " )"
            ]

        Text (Control _ (  _, value ) _) ->
            [
                "Tron.text \"" ++ value ++ "\""
            ]

        Color (Control _ (  _, value ) _) ->
            case Color.toRgba value of
                { red, green, blue, alpha } ->
                    [
                        "Tron.color"
                        , "    <| Color.fromRgba"
                        , "        { red = " ++ String.fromFloat red
                        , "        , green = " ++ String.fromFloat green
                        , "        , blue = " ++ String.fromFloat blue
                        , "        , alpha = " ++ String.fromFloat alpha
                        , "        }"
                    ]

        Toggle (Control _ value _) ->
            [
                "Tron.toggle "
                    ++ if V.toggleToBool value then "True" else "False"
            ]

        Action _ ->
            [
                "Tron.button"
            ]

        Group _ _ (Control items { form, face } _) ->
            "Tron.nest"
                :: (list labelAndPropToLines (Array.toList items) |> indent 4)

        Live prop_ ->
            propToLines prop_ ++ [ "|> live" ]

        _ -> []


labelAndPropToLines : ( String, Tron () ) -> List String
labelAndPropToLines ( label, prop ) =
    [ "( \"" ++ label ++ "\""
    , "  , " ]
    ++ (propToLines prop |> indent 4)
    ++ [ "  )" ]


addComma : List String -> List String
addComma strings =
    case strings of
        [] -> strings
        first_::rest_
            -> (", " ++ first_) :: rest_


list : (a -> List String) -> List a -> List String
list f l =
    case l of
        [] -> [ "[", "]" ]
        first::rest ->
            ("[ " ++ (List.head (f first) |> Maybe.withDefault ""))
            :: (List.tail (f first) |> Maybe.withDefault [])
            ++ (rest |> List.map f |> List.map addComma |> List.concat)
            ++ [ "]" ]


indent : Int -> List String -> List String
indent n strings =
    strings
        |> List.map2 (++)
            (List.repeat (List.length strings) (List.repeat n " " |> String.join ""))