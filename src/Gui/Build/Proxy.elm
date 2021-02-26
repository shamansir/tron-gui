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


choice
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
choice pShape cShape toLabel items current compare =
    B.choice pShape cShape toLabel items current compare FromChoice


choiceIcons
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
choiceIcons pShape cShape toLabel items current compare =
    B.choiceIcons pShape cShape toLabel items current compare (toLabel >> Tuple.first)


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder ProxyValue
choiceAuto pShape cShape toLabel items current =
    B.choiceAuto pShape cShape toLabel items current toLabel



strings
     : List String
    -> String
    -> B.Builder ProxyValue
strings options current =
    B.strings options current identity


labels
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ProxyValue
labels toLabel items current compare =
    B.labels toLabel items current compare toLabel


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder ProxyValue
labelsAuto toLabel items current =
    B.labelsAuto toLabel items current toLabel


palette
     : PanelShape
    -> List Color
    -> Color
    -> B.Builder ProxyValue
palette shape options current =
    B.palette shape options current Color.toCssString
