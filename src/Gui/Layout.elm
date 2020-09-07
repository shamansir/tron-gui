module Gui.Layout exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Over exposing (Over, Path)
import BinPack exposing (..)


maxCellsByX = 10
maxCellsByY = 10


type alias Layout = BinPack Path


getSize : Layout -> ( Int, Int )
getSize _ = ( maxCellsByX, maxCellsByY )


init : Layout
init = container maxCellsByX maxCellsByY


pack : Over msg -> Layout
pack over = container 0 0
