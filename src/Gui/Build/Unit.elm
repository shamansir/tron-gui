module Gui.Build.Unit exposing (..)


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
import Gui.Control.Nest exposing (Form(..))


type alias Builder = B.Builder ()


type alias Set = B.Set ()


none : Builder
none = B.none


root : Set -> Builder
root = B.root


float : Axis -> Float -> Builder
float axis current = B.float axis current <| always ()


int : { min: Int, max : Int, step : Int } -> Int -> Builder
int axis current = B.int axis current <| always ()


number : Axis -> Float -> Builder
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> Builder
xy xAxis yAxis = B.xy xAxis yAxis <| always ()


coord : ( Axis, Axis ) -> ( Float, Float ) -> Builder
coord = xy


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> Builder
input toString fromString current = B.input toString fromString current <| always ()


text : String -> Builder
text default = B.text default <| always ()


color : Color -> Builder
color current = B.color current <| always ()


button : Builder
button = B.button <| always ()


buttonWith : Icon -> Builder
buttonWith icon_ = B.buttonWith icon_ <| always ()


toggle : Bool -> Builder
toggle current = B.toggle current <| always ()


bool : Bool -> Builder
bool = toggle


nest : PanelShape -> CellShape -> Set -> Builder
nest = B.nest


choice
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choice pShape cShape toLabel items current compare =
    B.choice pShape cShape toLabel items current compare <| always ()


choiceIcons
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
choiceIcons pShape cShape toLabel items current compare =
    B.choiceIcons pShape cShape toLabel items current compare <| always ()


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
choiceAuto pShape cShape toLabel items current =
    B.choiceAuto pShape cShape toLabel items current <| always ()



strings
     : List String
    -> String
    -> Builder
strings options current =
    B.strings options current <| always ()


labels
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> Builder
labels toLabel items current compare =
    B.labels toLabel items current compare <| always ()


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> Builder
labelsAuto toLabel items current =
    B.labelsAuto toLabel items current <| always ()


palette
     : PanelShape
    -> List Color
    -> Color
    -> Builder
palette shape options current =
    B.palette shape options current <| always ()


icon : String -> Icon
icon = Icon
