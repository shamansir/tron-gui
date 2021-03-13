module Gui.Build.String exposing (..)


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
import Gui.Control.Toggle exposing (boolToToggle, toggleToBool, toggleToString)
import Gui.Control.XY exposing (xyToString, xyFromString)
import Gui.Control.Nest exposing (Form(..), ItemId)


none : B.Builder String
none = B.none


root : B.Set String -> B.Builder String
root = B.root


float : Axis -> Float -> B.Builder String
float axis current = B.float axis current <| String.fromFloat


int : { min: Int, max : Int, step : Int } -> Int -> B.Builder String
int axis current = B.int axis current <| String.fromInt


number : Axis -> Float -> B.Builder String
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> B.Builder String
xy xAxis yAxis = B.xy xAxis yAxis xyToString


coord : ( Axis, Axis ) -> ( Float, Float ) -> B.Builder String
coord = xy


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> B.Builder String
input toString fromString current = B.input toString fromString current toString


text : String -> B.Builder String
text default = B.text default identity


color : Color -> B.Builder String
color current = B.color current Color.toCssString


button : B.Builder String
button = B.button <| always ""


buttonWith : Icon -> B.Builder String
buttonWith icon = B.buttonWith icon <| always ""


toggle : Bool -> B.Builder String
toggle current = B.toggle current (boolToToggle >> toggleToString)


bool : Bool -> B.Builder String
bool = toggle


nest : PanelShape -> CellShape -> B.Set String -> B.Builder String
nest = B.nest


choice
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder String
choice pShape cShape toLabel items current compare =
    B.choice pShape cShape toLabel items current compare toLabel


choiceIcons
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder String
choiceIcons pShape cShape toLabel items current compare =
    B.choiceIcons pShape cShape toLabel items current compare (toLabel >> Tuple.first)


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder String
choiceAuto pShape cShape toLabel items current =
    B.choiceAuto pShape cShape toLabel items current toLabel



strings
     : List String
    -> String
    -> B.Builder String
strings options current =
    B.strings options current identity


labels
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder String
labels toLabel items current compare =
    B.labels toLabel items current compare toLabel


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder String
labelsAuto toLabel items current =
    B.labelsAuto toLabel items current toLabel


palette
     : PanelShape
    -> List Color
    -> Color
    -> B.Builder String
palette shape options current =
    B.palette shape options current Color.toCssString
