module Gui.Build.Proxy exposing (..)


import Gui.Build as B

import Array
import Color exposing (Color)
import Axis exposing (Axis)

import Gui.Control exposing (..)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (expand, collapse)
import Gui.Control exposing (Control(..))
import Gui.Util exposing (findMap)
import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CS
import Gui.Style.PanelShape exposing (PanelShape)
import Gui.Style.PanelShape as Shape exposing (find, rows, cols)

-- TODO: make controls init themselves, so get rid of these imports below
import Gui.Control.Text exposing (TextState(..))
import Gui.Control.Button exposing (Face(..), Icon(..))
import Gui.Control.Toggle exposing (boolToToggle, toggleToBool)
import Gui.Control.Nest exposing (NestState(..), SelectedAt(..))

import Gui.ProxyValue exposing (ProxyValue(..))


none : B.Builder ProxyValue
none = B.none


root : B.Set ProxyValue -> B.Builder ProxyValue
root = B.root


float : Axis -> Float -> B.Builder ProxyValue
float axis default = B.float axis default FromSlider


int : { min: Int, max : Int, step : Int } -> Int -> B.Builder ProxyValue
int axis default = B.int axis default (toFloat >> FromSlider)


number : Axis -> Float -> B.Builder ProxyValue
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> B.Builder ProxyValue
xy xAxis yAxis = B.xy xAxis yAxis FromXY


coord : ( Axis, Axis ) -> ( Float, Float ) -> B.Builder ProxyValue
coord = xy


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> B.Builder ProxyValue
input toString fromString current = B.input toString fromString current (toString >> FromInput)


text : String -> B.Builder ProxyValue
text default = B.text default FromInput


color : Color -> B.Builder ProxyValue
color current = B.color current FromColor


button : B.Builder ProxyValue
button = B.button <| always FromButton


buttonWith : Icon -> B.Builder ProxyValue
buttonWith icon = B.buttonWith icon <| always FromButton


toggle : Bool -> B.Builder ProxyValue
toggle current = B.toggle current (boolToToggle >> FromToggle)


bool : Bool -> B.Builder ProxyValue
bool = toggle


nest : PanelShape -> CellShape -> B.Set ProxyValue -> B.Builder ProxyValue
nest = B.nest


choice -- TODO: remove, make choicesAuto default, change to List ( a, Label )
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
choice pShape cShape toLabel =
    choiceHelper
        ( pShape, cShape )
        (\callByIndex index val ->
            ( toLabel val
            , B.button <| always <| callByIndex <| SelectedAt index
            )
        )


choiceIcons -- TODO: remove, make choicesAuto default, change to List ( a, Label, Icon )
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
choiceIcons pShape cShape toLabelAndIcon =
    choiceHelper
        ( pShape, cShape )
        (\callByIndex index val ->
            let ( label, theIcon ) = toLabelAndIcon val
            in
                ( label
                , B.buttonWith theIcon <| always <| callByIndex <| SelectedAt index
                )
        )


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder ProxyValue
choiceAuto pShape cShape f items v =
    choice pShape cShape f items v (==)



strings
     : List String
    -> String
    -> B.Builder ProxyValue
strings options current =
    choice
        (cols 1)
        CS.twiceByHalf
        identity
        options
        current
        ((==))


labels -- TODO: remove, make labelsAuto default
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
labels toLabel options current compare =
    choice
        (cols 1)
        CS.twiceByHalf
        toLabel
        options
        current
        compare


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder ProxyValue
labelsAuto toLabel options current =
    labels toLabel options current (==)


palette
     : PanelShape
    -> List Color
    -> Color
    -> B.Builder ProxyValue
palette shape options current =
    choiceHelper
        ( shape, CS.half )
        (\callByIndex index val ->
            ( Color.toCssString val
            , B.colorButton val <| always <| callByIndex <| SelectedAt index
            )
        )
        options
        current
        (\cv1 cv2 ->
            case ( cv1 |> Color.toRgba, cv2 |> Color.toRgba ) of
                ( c1, c2 ) ->
                    (c1.red == c2.red) &&
                    (c1.blue == c2.blue) &&
                    (c1.green == c2.green) &&
                    (c1.alpha == c2.alpha)
        )


choiceHelper
     : ( PanelShape, CellShape )
    -> ( (SelectedAt -> ProxyValue) -> Int -> a -> ( Label, B.Builder ProxyValue ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
choiceHelper ( shape, cellShape ) toBuilder options current compare =
    let
        indexedOptions = options |> List.indexedMap Tuple.pair
        callByIndex (SelectedAt indexToCall) =
            FromChoice indexToCall
        set =
            options
                |> List.indexedMap (toBuilder callByIndex)
    in
        Choice
            Nothing
            ( findShape cellShape shape (set |> List.map Tuple.second)
            , cellShape
            )
            <| Control
                ( set |> Array.fromList )
                ( Collapsed
                ,
                    indexedOptions
                        -- FIXME: searching for the item every time seems wrong
                        |> findMap
                            (\(index, option) ->
                                if compare option current
                                    then Just index
                                    else Nothing
                            )
                        |> Maybe.withDefault 0
                        |> SelectedAt
                )
                (Just <| Tuple.second >> callByIndex)
