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
import Gui.Control.Nest exposing (NestState(..), SelectedAt(..))


none : B.Builder ()
none = B.none


root : B.Set String -> B.Builder ()
root = B.root


float : Axis -> Float -> B.Builder ()
float axis current = B.float axis current <| always ()


int : { min: Int, max : Int, step : Int } -> Int -> B.Builder ()
int axis current = B.int axis current <| always ()


number : Axis -> Float -> B.Builder ()
number = float


xy : ( Axis, Axis ) -> ( Float, Float ) -> B.Builder ()
xy xAxis yAxis = B.xy xAxis yAxis xyToString


coord : ( Axis, Axis ) -> ( Float, Float ) -> B.Builder ()
coord = xy


input : ( a -> String ) -> ( String -> Maybe a ) -> a -> B.Builder ()
input toString fromString current = B.input toString fromString current <| always ()


text : String -> B.Builder ()
text default = B.text default identity


color : Color -> B.Builder ()
color current = B.color current <| always ()


button : B.Builder ()
button = B.button <| always ""


buttonWith : Icon -> B.Builder ()
buttonWith icon = B.buttonWith icon <| always ""


toggle : Bool -> B.Builder ()
toggle current = B.toggle current <| always ()


bool : Bool -> B.Builder ()
bool = toggle


nest : PanelShape -> CellShape -> B.Set String -> B.Builder ()
nest = B.nest


choice
     : PanelShape
    -> CellShape
    -> ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ()
choice pShape cShape toLabel items current compare =
    B.choice pShape cShape toLabel items current compare <| always ()


choiceIcons
     : PanelShape
    -> CellShape
    -> ( a -> ( Label, Icon ) )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ()
choiceIcons pShape cShape toLabel items current compare =
    B.choiceIcons pShape cShape toLabel items current compare <| always ()


choiceAuto
     : PanelShape
    -> CellShape
    -> ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder ()
choiceAuto pShape cShape toLabel items current =
    B.choiceAuto pShape cShape toLabel items current <| always ()



strings
     : List String
    -> String
    -> B.Builder ()
strings options current =
    B.strings options current identity


labels
     : ( a -> Label )
    -> List a
    -> a
    -> ( a -> a -> Bool )
    -> B.Builder ()
labels toLabel items current compare =
    B.labels toLabel items current compare <| always ()


labelsAuto
     : ( comparable -> Label )
    -> List comparable
    -> comparable
    -> B.Builder ()
labelsAuto toLabel items current =
    B.labelsAuto toLabel items current <| always ()


palette
     : PanelShape
    -> List Color
    -> Color
    -> B.Builder ()
palette shape options current =
    B.palette shape options current <| always ()
