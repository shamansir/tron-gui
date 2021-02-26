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
import Gui.Control.Toggle exposing (boolToToggle, toggleToBool)
import Gui.Control.Nest exposing (NestState(..), SelectedAt(..))


none : B.Builder String
none = B.none


root : B.Set String -> B.Builder String
root = B.root


float : Axis -> Float -> B.Builder String
float axis default = B.float axis default <| String.fromFloat


int : { min: Int, max : Int, step : Int } -> Int -> B.Builder String
int axis default = B.int axis default <| String.fromInt


number : Axis -> Float -> B.Builder String
number = float
