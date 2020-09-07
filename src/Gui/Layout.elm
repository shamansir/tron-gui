module Gui.Layout exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Property exposing (Property, Path)
import Gui.Property as Property exposing (fold)
import BinPack exposing (..)
import BinPack as Layout exposing (..)


maxCellsByX = 10
maxCellsByY = 10


type alias Layout = BinPack Path


getSize : Layout -> ( Int, Int )
getSize _ = ( maxCellsByX, maxCellsByY )


init : Layout
init = container maxCellsByX maxCellsByY


pack : Property msg -> Layout
pack =
    Property.fold
        (\path prop layout ->
            layout
                |> Layout.pack ( { width = 1, height = 1 }, path )
                |> Maybe.withDefault layout
        )
        init
