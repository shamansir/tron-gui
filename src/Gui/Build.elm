module Gui.Build exposing (..)


import Array

import Gui.Control exposing (..)
import Gui.Over exposing (..)
import Gui.Control exposing (Control(..))
import Gui.Util exposing (findMap)


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
nest = nestIn ( 3, 3 )


nestIn : Shape -> List (Label, Over msg) -> (ExpandState -> msg) -> Over msg
nestIn shape items handler =
    Group
        <| Control
            ( shape
            , Array.fromList items
            )
            ( Collapsed
            , Nothing
            )
            (Tuple.first >> handler)


choice
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Over msg
choice toLabel options current compare toMsg =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
    in
        Choice
            <| Control
                (options
                    |> List.map toLabel
                    |> List.map (Tuple.pair Nothing)
                    |> Array.fromList)
                (indexedOptions
                    |> findMap
                        (\(index, option) ->
                            if compare option current
                                then Just index
                                else Nothing
                        )
                    |> Maybe.withDefault 0
                )
                (\selectedIndex ->
                    indexedOptions
                        |> findMap
                            (\(index, option) ->
                                if selectedIndex == index
                                    then Just option
                                    else Nothing
                            )
                        |> Maybe.map toMsg
                        |> Maybe.withDefault (toMsg current)
                )


strings
     : List String
    -> String
    -> ( String -> msg )
    -> Over msg
strings options current toMsg =
    choice
        identity
        options
        current
        ((==))
        toMsg


-- TODO:


-- expand : Over msg -> Over msg


-- collapse : Over msg -> Over msg


-- toggleOn : Over msg -> Over msg


-- toggleOff : Over msg -> Over msg


-- reshape : Shape -> Over msg -> Over msg
