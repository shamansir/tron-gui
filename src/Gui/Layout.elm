module Gui.Layout exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Control exposing (Control(..))
import Gui.Property exposing (Property(..), Path(..), ExpandState(..))
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
                , y = pos.y - bounds.y
                }
                innerLayout
                |> Maybe.map Tuple.first
        Nothing ->
            Nothing


pack : Property msg -> Layout
pack prop =
    case prop of
        Group (Control (shape, items) _ _) ->
            let
                firstLevel =
                    items
                        |> Array.indexedMap Tuple.pair
                        |> Array.foldl
                            (\(index, _) layout ->
                                layout |> packOne [index]
                            )
                            init
                packOne path layout =
                    layout
                        |> BinPack.pack1
                            ( { width = 1, height = 1 }
                            , One <| Path path
                            )
                packOne1 path layout =
                    layout
                        |> BinPack.pack1
                            ( { width = 1, height = 1 }
                            , Path path
                            )
                packPlate path (w, h) plateItems layout =
                    let
                        platePacked =
                            layout
                                |> BinPack.pack1
                                    (
                                        { width = toFloat w
                                        , height = toFloat h
                                        }
                                    , Plate
                                        <| Array.foldl
                                            (\(index, ( _, innerProp)) plateLayout ->
                                                packOne1 (path ++ [index]) plateLayout
                                            )
                                            (BinPack.container (toFloat w) (toFloat h))
                                        <| Array.indexedMap Tuple.pair
                                        <| plateItems
                                    )
                    in
                        platePacked
                        -- plateItems |> packPlatesOf path platePacked
                packPlatesOf path layout =
                    Array.indexedMap Tuple.pair
                        >> Array.foldl
                            (\(index, ( _, innerProp) ) prevLayout ->
                                let
                                    nextPath = path ++ [index]
                                in
                                case innerProp of
                                    Choice (Control ( innerShape, innerItems ) (expanded, _) _) ->
                                        case expanded of
                                            Expanded ->
                                                let
                                                    withPlate
                                                        = prevLayout
                                                            |> packPlate
                                                                nextPath
                                                                innerShape
                                                                innerItems
                                                in
                                                    packPlatesOf nextPath withPlate innerItems
                                            Collapsed -> prevLayout
                                    Group (Control ( innerShape, innerItems ) (expanded, _) _) ->
                                        case expanded of
                                            Expanded ->
                                                let
                                                    withPlate
                                                        = prevLayout
                                                            |> packPlate
                                                                nextPath
                                                                innerShape
                                                                innerItems
                                                in
                                                    packPlatesOf nextPath withPlate innerItems
                                            Collapsed -> prevLayout
                                    _ ->
                                        prevLayout
                            )
                            layout
            in
                items |> packPlatesOf [] firstLevel |> Debug.log "layout"
        _ ->
            init
                |> BinPack.pack ( { width = 1, height = 1 }, One <| Path [] )
                |> Maybe.withDefault init
