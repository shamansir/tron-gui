module Gui.Build exposing (..)


import Array

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Control exposing (Control(..))
import Gui.Util exposing (findMap)


none : Property msg
none = Nil


float : Axis -> Float -> ( Float -> msg ) -> Property msg
float axis default =
    Number
        << Control axis default -- RoundBy 2


int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Property msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Property msg
xy axes default =
    Coordinate
        << Control axes default


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Property msg
input toString fromString default toMsg =
    Text
        <| Control
            ()
            (toString default)
            (fromString >> Maybe.withDefault default >> toMsg)


text : String -> (String -> msg) -> Property msg
text default =
    Text
        << Control
            ()
            default


color : Color -> (Color -> msg) -> Property msg
color default =
    Color
        << Control
            ()
            default


button : (() -> msg) -> Property msg
button =
    Action
        << Control
            Nothing
            ()


toggle : ToggleState -> (ToggleState -> msg) -> Property msg
toggle default =
    Toggle
        << Control
            ()
            default


-- FIXME: get rid of the handler having almost no sense
nest : List (Label, Property msg) -> (ExpandState -> msg) -> Property msg
nest = nestIn ( 3, 3 )


-- FIXME: get rid of the handler having almost no sense
nestIn : Shape -> List (Label, Property msg) -> (ExpandState -> msg) -> Property msg
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
    -> Property msg
choice =
    choiceIn (3, 3)


choiceIn
     : Shape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Property msg
choiceIn shape toLabel options current compare toMsg =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex indexToCall =
            indexedOptions
                |> findMap
                    (\(index, option) ->
                        if indexToCall == index
                            then Just option
                            else Nothing
                    )
                |> Maybe.map toMsg
                |> Maybe.withDefault (toMsg current)
    in
        Choice
            <| Control
                ( shape
                , options
                    |> List.map toLabel
                    |> List.indexedMap
                        (\index label ->
                            ( label
                            , button <| always <| callByIndex index
                            )
                        )
                    |> Array.fromList
                )
                ( Collapsed
                , indexedOptions
                    |> findMap
                        (\(index, option) ->
                            if compare option current
                                then Just index
                                else Nothing
                        )
                    |> Maybe.withDefault 0
                )
                (Tuple.second >> callByIndex)


strings
     : List String
    -> String
    -> ( String -> msg )
    -> Property msg
strings options current toMsg =
    choice
        identity
        options
        current
        ((==))
        toMsg


expand : Property msg -> Property msg
expand prop =
    case prop of
        Group ( Control setup ( _, focus ) handler ) ->
            Group ( Control setup ( Expanded, focus ) handler )
        Choice ( Control setup ( _, selection ) handler ) ->
            Choice ( Control setup ( Expanded, selection ) handler )
        _ -> prop


collapse : Property msg -> Property msg
collapse prop =
    case prop of
        Group ( Control setup ( _, focus ) handler ) ->
            Group ( Control setup ( Collapsed, focus ) handler )
        Choice ( Control setup ( _, selection ) handler ) ->
            Choice ( Control setup ( Collapsed, selection ) handler )
        _ -> prop


toggleOn : Property msg -> Property msg
toggleOn prop =
    case prop of
        Toggle ( Control setup _ handler ) ->
            Toggle ( Control setup TurnedOn handler )
        _ -> prop


toggleOff : Property msg -> Property msg
toggleOff prop =
    case prop of
        Toggle ( Control setup _ handler ) ->
            Toggle ( Control setup TurnedOff handler )
        _ -> prop


reshape : Shape -> Property msg -> Property msg
reshape shape prop =
    case prop of
        Group ( Control ( _, items ) ( expanded, focus ) handler ) ->
            Group ( Control ( shape, items ) ( expanded, focus ) handler )
        _ -> prop
