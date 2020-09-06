module Gui.Build exposing (..)


import Array

import Gui.Control exposing (..)
import Gui.Over exposing (..)
import Gui.Control exposing (Control(..))


none : Over msg
none = Nil


float : Axis -> Float -> ( Float -> msg ) -> Over msg
float axis default =
    Number
        << Control axis default -- RoundBy 2


int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Over msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Over msg
xy axes default =
    Coordinate
        << Control axes default


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Over msg
input toString fromString default toMsg =
    Text
        <| Control
            ()
            (toString default)
            (fromString >> Maybe.withDefault default >> toMsg)


text : String -> (String -> msg) -> Over msg
text default =
    Text
        << Control
            ()
            default


color : Color -> (Color -> msg) -> Over msg
color default =
    Color
        << Control
            ()
            default


button : (() -> msg) -> Over msg
button =
    Action
        << Control
            Nothing
            ()


toggle : ToggleState -> (ToggleState -> msg) -> Over msg
toggle default =
    Toggle
        << Control
            ()
            default


-- FIXME: get rid of the handler having almost no sense
nest : List (Label, Over msg) -> (ExpandState -> msg) -> Over msg
nest items handler =
    Group
        <| Control
            { shape = (3, 3)
            , items = Array.fromList items
            }
            { expanded = Collapsed
            , focus = Nothing
            }
            (.expanded >> handler)


-- TODO:


-- expand : Over msg -> Over msg


-- collapse : Over msg -> Over msg


-- toggleOn : Over msg -> Over msg


-- toggleOff : Over msg -> Over msg


-- reshape : Shape -> Over msg -> Over msg
