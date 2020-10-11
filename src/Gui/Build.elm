module Gui.Build exposing (..)


import Array
import Color exposing (Color)

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Control exposing (Control(..))
import Gui.Util exposing (findMap)


type alias Builder msg = Property msg


none : Builder msg
none = Nil


float : Axis -> Float -> ( Float -> msg ) -> Builder msg
float axis default =
    Number
        << Control axis default -- RoundBy 2


int : { min: Int, max : Int, step : Int } -> Int -> ( Int -> msg ) -> Builder msg
int { min, max, step } default toMsg =
    float
        { min = toFloat min, max = toFloat max, step = toFloat step } -- RoundBy 0
        (toFloat default)
        (round >> toMsg)


xy : ( Axis, Axis ) -> ( Float, Float ) -> ( ( Float, Float ) -> msg ) -> Builder msg
xy axes default =
    Coordinate
        << Control axes default


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> ( a -> msg ) -> Builder msg
input toString fromString default toMsg =
    Text
        <| Control
            ()
            ( Ready, toString default)
            (Tuple.second >> fromString >> Maybe.withDefault default >> toMsg)


text : String -> (String -> msg) -> Builder msg
text default handler =
    Text
        <| Control
            ()
            (Ready, default)
            (Tuple.second >> handler)


color : Color -> (Color -> msg) -> Builder msg
color default =
    Color
        << Control
            ()
            default


button : (() -> msg) -> Builder msg
button =
    Action
        << Control
            Nothing
            ()


icon : String -> Icon
icon = Icon


button1 : Icon -> (() -> msg) -> Builder msg
button1 icon_ =
    Action
        << Control
            (Just icon_)
            ()


toggle : ToggleState -> (ToggleState -> msg) -> Builder msg
toggle default =
    Toggle
        << Control
            ()
            default


-- FIXME: get rid of the handler having almost no sense
nest : List (Label, Builder msg) -> (GroupState -> msg) -> Builder msg
nest items =
    nestIn (findShape items) items


root : List (Label, Builder msg) -> (GroupState -> msg) -> Builder msg
root props =
    nestIn ( 1, List.length <| List.filter (Tuple.second >> isGhost >> not) props ) props
        >> expand


-- FIXME: get rid of the handler having almost no sense
nestIn : Shape -> List (Label, Builder msg) -> (GroupState -> msg) -> Builder msg
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
    -> Builder msg
choice f items =
    choiceIn (findShape items) f items


choiceIn
     : Shape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> ( a -> msg )
    -> Builder msg
choiceIn shape toLabel options current compare toMsg =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex (Selected indexToCall) =
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
                            , button <| always <| callByIndex <| Selected index
                            )
                        )
                    |> Array.fromList
                )
                ( Collapsed
                ,
                    ( Nothing
                    , indexedOptions
                        |> findMap
                            (\(index, option) ->
                                if compare option current
                                    then Just index
                                    else Nothing
                            )
                        |> Maybe.withDefault 0
                        |> Selected
                    )
                )
                (Tuple.second >> Tuple.second >> callByIndex)


strings
     : List String
    -> String
    -> ( String -> msg )
    -> Builder msg
strings options current toMsg =
    choice
        identity
        options
        current
        ((==))
        toMsg


findShape : List a -> ( Int, Int )
findShape items =
    ( 1, List.length items )
