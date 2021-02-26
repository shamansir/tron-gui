module Gui.Build.Proxy exposing (..)


import Gui.Build as B

import Gui.Expose exposing (ProxyValue(..))

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
