module Gui.Layout exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Control exposing (..)
import BinPack exposing (..)


maxCellsByX = 10
maxCellsByY = 10


type alias Layout = BinPack Path


getSize : Layout -> ( Int, Int )
getSize _ = ( maxCellsByX, maxCellsByY )
