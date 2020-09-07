module Gui.Layout exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Property exposing (Property, Path)
import Gui.Property as Property exposing (fold)
import BinPack exposing (..)


maxCellsByX = 10
maxCellsByY = 10


type Cell
    = One Path
    | Plate (BinPack Path)


type alias Layout = BinPack Cell


getSize : Layout -> ( Int, Int )
getSize _ = ( maxCellsByX, maxCellsByY )


init : Layout
init = container maxCellsByX maxCellsByY


find : { x : Float, y : Float } -> Layout -> Maybe Path
find pos layout =
    case BinPack.find pos layout of
        Just ( One path, _ ) ->
            Just path
        Just ( Plate innerLayout, bounds ) ->
            BinPack.find
                { x = pos.x - bounds.x
                , y = pos.y - bounds.y }
                innerLayout
                |> Maybe.map Tuple.first
        Nothing ->
            Nothing


pack : Property msg -> Layout
pack =
    Property.fold
        (\path prop layout ->
            layout
                |> BinPack.pack ( { width = 1, height = 1 }, One path )
                |> Maybe.withDefault layout
        )
        init
