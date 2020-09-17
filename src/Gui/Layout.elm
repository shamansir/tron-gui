module Gui.Layout exposing (..)


import Array exposing (..)
import Json.Decode as Json


import Gui.Control exposing (Control(..))
import Gui.Path as Path exposing (Path)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (fold)
import BinPack exposing (..)


maxCellsByX = 9
maxCellsByY = 5


type Cell
    = One Path
    | Plate Path (BinPack Path)


type alias Layout = BinPack Cell


getSize : Layout -> ( Int, Int )
getSize _ = ( maxCellsByX, maxCellsByY )


init : Layout
init = container maxCellsByX maxCellsByY


find : Layout -> { x : Float, y : Float } -> Maybe Path
find layout pos =
    case BinPack.find pos layout of
        Just ( One path, _ ) ->
            Just path
        Just ( Plate _ innerLayout, bounds ) ->
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

                packOne path =
                    BinPack.pack1
                        ( { width = 1, height = 1 }
                        , One <| Path.fromList path
                        )

                packOne1 path =
                    BinPack.pack1
                        ( { width = 1, height = 1 }
                        , Path.fromList path
                        )

                packPlate path (w, h) plateItems =
                    BinPack.pack1
                        (
                            { width = toFloat w
                            , height = toFloat h
                            }
                        , Plate
                            (Path.fromList path)
                            <| Array.foldl
                                (\(index, ( _, innerProp)) plateLayout ->
                                    packOne1 (path ++ [index]) plateLayout
                                )
                                (BinPack.container (toFloat w) (toFloat h))
                            <| Array.indexedMap Tuple.pair
                            <| plateItems
                        )

                packGroupControl
                     : List Int
                    -> Layout
                    -> Control
                            ( Shape, Array ( Label, Property msg ) )
                            ( ExpandState, a )
                            msg
                    -> Layout
                packGroupControl path layout (Control ( innerShape, innerItems ) (expanded, _) _) =
                    case expanded of
                        Expanded ->
                            let
                                withPlate
                                    = layout
                                        |> packPlate
                                            path
                                            innerShape
                                            innerItems
                            in
                                packPlatesOf path withPlate innerItems
                        Collapsed -> layout

                packPlatesOf path layout =
                    Array.indexedMap Tuple.pair
                        >> Array.foldl
                            (\(index, ( _, innerProp) ) prevLayout ->
                                let
                                    nextPath = path ++ [index]
                                in
                                case innerProp of
                                    Choice control ->
                                        control |> packGroupControl nextPath prevLayout
                                    Group control ->
                                        control |> packGroupControl nextPath prevLayout
                                    _ ->
                                        prevLayout
                            )
                            layout

            in
                items |> packPlatesOf [] firstLevel
        _ ->
            init
                |> BinPack.pack ( { width = 1, height = 1 }, One <| Path.start )
                |> Maybe.withDefault init
