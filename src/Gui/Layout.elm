module Gui.Layout exposing (Layout, Cell(..), init, pack, pack1, unfold, find, toList)


import Array exposing (..)
import Json.Decode as Json


import Bounds
import BinPack exposing (..)

import Gui.Control exposing (Control(..))
import Gui.Path as Path exposing (Path)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (fold)
import Gui.Style.Dock exposing (Dock)
import Gui.Style.Dock as Dock
import Gui.Style.CellShape exposing (CellShape)
import Gui.Style.CellShape as CS

import Gui.Control.Nest exposing (Form(..))
import Gui.Control.Nest as Nest exposing (getItems)


type Cell_ a
    = One_ a
    | Many_ a (BinPack a)


type Cell a
    = One a
    | Many a (List a)


type alias Layout = ( Dock, ( Float, Float ), BinPack (Cell_ Path) )


{-
type Cells = Cells

type Pixels = Pixels


type Size a = Size ( Int, Int )


type Position a = Position { x : Float, y : Float }
-}


init : Dock -> ( Float, Float ) -> Layout
init dock size =
    ( dock
    , size
    , initBinPack <| Dock.adaptSize dock size
    )


initBinPack : ( Float, Float ) -> BinPack a
initBinPack ( maxCellsByX, maxCellsByY )
    = container maxCellsByX maxCellsByY


find : Layout -> { x : Float, y : Float } -> Maybe Path
find ( dock, size, layout ) pos =
    let
        adaptedPos = Dock.adaptPosition dock size pos
    in
        case layout |> BinPack.find adaptedPos of
            Just ( One_ path, _ ) ->
                Just path
            Just ( Many_ _ innerLayout, bounds ) ->
                BinPack.find
                    { x = adaptedPos.x - bounds.x
                    , y = adaptedPos.y - bounds.y
                    }
                    innerLayout
                    |> Maybe.map Tuple.first
            Nothing ->
                Nothing


pack : Dock -> ( Float, Float ) -> Property msg -> Layout
pack dock size = pack1 dock size Path.start


pack1 : Dock -> ( Float, Float ) -> Path -> Property msg -> Layout
pack1 dock size rootPath prop =
    case prop of
        Nil ->
            init dock size
        Group _ ( shape, _ ) control ->
            ( dock
            , size
            , packItemsAtRoot (Dock.adaptSize dock size) rootPath shape
                <| Nest.getItems control
            )
        Choice _ ( shape, _ ) control ->
            ( dock
            , size
            , packItemsAtRoot (Dock.adaptSize dock size) rootPath shape
                <| Nest.getItems control
            )
        _ ->
            ( dock
            , size
            , initBinPack (Dock.adaptSize dock size)
                |> BinPack.carelessPack ( { width = 1, height = 1 }, One_ <| Path.start )
            )


packItemsAtRoot
    :  ( Float, Float )
    -> Path
    -> Shape
    -> Array (Label, Property msg)
    -> BinPack (Cell_ Path)
packItemsAtRoot size rp shape items =
    let
        rootPath = Path.toList rp

        firstLevel =
            items
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, (_, theProp)) layout ->
                        if not <| isGhost theProp
                            then layout |> packOne (rootPath ++ [index])
                            else layout
                    )
                    (initBinPack size)

        packOne path =
            BinPack.carelessPack
                ( { width = 1, height = 1 }
                , One_ <| Path.fromList path
                )

        packOneSub path cellShape =
            BinPack.carelessPack
                ( case CS.numify cellShape of
                    ( cw, ch ) ->
                        { width = cw, height = ch }
                , Path.fromList path
                )

        packMany path (w, h) cellShape plateItems =
            BinPack.carelessPack
                (
                    { width = w
                    , height = h
                    }
                , Many_
                    (Path.fromList path)
                    <| Array.foldl
                        (\(index, ( _, innerProp)) plateLayout ->
                            if not <| isGhost innerProp
                                then packOneSub (path ++ [index]) cellShape plateLayout
                                else plateLayout
                        )
                        (BinPack.container w h)
                    <| Array.indexedMap Tuple.pair
                    <| plateItems
                )

        packGroupControl
            :  List Int
            -> ( Shape, CellShape )
            -> BinPack (Cell_ Path)
            -> Control
                    ( Array ( Label, Property msg ) )
                    { a | form : Form }
                    msg
            -> BinPack (Cell_ Path)
        packGroupControl
            path
            ( innerShape, cellShape )
            layout
            control =
            if Nest.is Expanded control then
                let
                    withPlate
                        = layout
                            |> packMany
                                path
                                innerShape
                                cellShape
                                (Nest.getItems control)
                in
                    packPlatesOf path withPlate <| Nest.getItems control
            else layout

        packPlatesOf path layout =
            Array.indexedMap Tuple.pair
                >> Array.foldl
                    (\(index, ( _, innerProp) ) prevLayout ->
                        let
                            nextPath = path ++ [index]
                        in
                        case innerProp of
                            Choice _ innerShape control ->
                                control |> packGroupControl nextPath innerShape prevLayout
                            Group _ innerShape control ->
                                control |> packGroupControl nextPath innerShape prevLayout
                            _ ->
                                prevLayout
                    )
                    layout

    in
        items |> packPlatesOf rootPath firstLevel


unfold : ( Cell ( Path, Bounds ) -> a -> a ) -> a -> Layout -> a
unfold f def ( dock, size, bp ) =
    let
        adaptBounds =
            Dock.adaptBounds dock <| Dock.adaptSize dock size
    in
        BinPack.foldGeometry
            (\( c, bounds ) prev ->
                case c of
                    One_ path ->
                        f
                            ( One ( path, adaptBounds bounds ) )
                            prev

                    Many_ path innerBp ->
                        f
                            ( Many
                                ( path, adaptBounds bounds )
                                <| List.map
                                    (Tuple.mapSecond
                                        <| adaptBounds << Bounds.shift bounds
                                    )
                                <| BinPack.foldGeometry
                                    (::)
                                    []
                                    innerBp
                            )
                            prev
            )
            def
            bp


toList : Layout -> List ( Cell ( Path, Bounds ) )
toList =
    unfold (::) []

