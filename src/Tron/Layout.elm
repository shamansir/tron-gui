module Tron.Layout exposing (Layout, Cell(..), init, pack, pack1, unfold, find, toList)


import Array exposing (..)
import Json.Decode as Json


import Bounds
import BinPack exposing (..)
import Size exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Path as Path exposing (Path)
import Tron.Property exposing (..)
import Tron.Property as Property exposing (fold)
import Tron.Style.Dock exposing (Dock)
import Tron.Style.Dock as Dock
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape exposing (PanelShape)
import Tron.Pages as Pages exposing (Pages, PageNum)

import Tron.Control.Nest exposing (Form(..))
import Tron.Control.Nest as Nest exposing (getItems)



type Cell_ a
    = One_ a
    | Many_ a (Pages (BinPack a))


type Cell a
    = One a
    | Many a (Pages (List a))


type alias Layout = ( Dock, SizeF Cells, BinPack (Cell_ Path) )


type Position a = Position { x : Float, y : Float }


init : Dock -> SizeF Cells -> Layout
init dock size =
    ( dock
    , size
    , initBinPack <| Dock.adaptSize dock size
    )


initBinPack : SizeF Cells -> BinPack a
initBinPack (SizeF ( maxCellsByX, maxCellsByY ))
    = container maxCellsByX maxCellsByY


find : Layout -> { x : Float, y : Float } -> Maybe Path
find ( dock, size, layout ) pos =
    let
        adaptedPos = Dock.adaptPosition dock size pos
    in
        case layout |> BinPack.find adaptedPos of
            Just ( One_ path, _ ) ->
                Just path
            Just ( Many_ _ innerPages, bounds ) ->
                innerPages
                    |> Pages.getCurrent
                    |> Maybe.andThen
                        (BinPack.find
                            { x = adaptedPos.x - bounds.x
                            , y = adaptedPos.y - bounds.y
                            }
                        )
                    |> Maybe.map Tuple.first
            Nothing ->
                Nothing


pack : Dock -> SizeF Cells -> Property msg -> Layout
pack dock size = pack1 dock size Path.start


pack1 : Dock -> SizeF Cells -> Path -> Property msg -> Layout
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
    :  SizeF Cells
    -> Path
    -> PanelShape
    -> Array (Label, Property msg)
    -> BinPack (Cell_ Path)
packItemsAtRoot size rp shape items =
    let
        rootPath = Path.toList rp

        firstLevel =
            items
                |> Array.indexedMap Tuple.pair -- FIXME: consider pages
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

        packMany path pageNum panelShape cellShape plateItems =
            let
                ( pageCount, SizeF ( pageWidth, pageHeight ) ) =
                    Property.findShape panelShape cellShape <| Array.toList plateItems
            in

                BinPack.carelessPack
                    (
                        { width = pageWidth
                        , height = pageHeight
                        }
                    , Many_
                            (Path.fromList path)
                            <| Pages.map
                                (List.foldl
                                    (\(index, innerProp) plateLayout ->
                                        if not <| isGhost innerProp
                                            then packOneSub (path ++ [index]) cellShape plateLayout
                                            else plateLayout
                                    )
                                    (BinPack.container pageWidth pageHeight)
                                )
                            <| Pages.switchTo pageNum
                            <| Pages.distribute 9 -- FIXME: floor (w * h)
                            <| Array.toList
                            <| Array.indexedMap Tuple.pair
                            <| plateItems
                    )

        packGroupControl
            :  List Int
            -> ( PanelShape, CellShape )
            -> BinPack (Cell_ Path)
            -> Control
                    ( Array ( Label, Property msg ) )
                    { a | form : Form, page : PageNum }
                    msg
            -> BinPack (Cell_ Path)
        packGroupControl
            path
            ( panelShape, cellShape )
            layout
            control =
            if Nest.is Expanded control then
                let
                    items_ = Nest.getItems control

                    withPlate
                        = layout
                            |> packMany
                                path
                                (Nest.getPage control)
                                panelShape
                                cellShape
                                (items_ |> Array.map Tuple.second)
                in
                    packPlatesOf path withPlate items_
            else layout

        packPlatesOf path layout =
            Array.indexedMap Tuple.pair -- FIXME: consider pages
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

                    Many_ path bpPages ->
                        f
                            ( Many
                                ( path, adaptBounds bounds )
                                <| Pages.map
                                    (List.map
                                        (Tuple.mapSecond
                                            <| adaptBounds << Bounds.shift bounds
                                        )
                                    << BinPack.foldGeometry
                                        (::)
                                        []
                                    )
                                <| bpPages
                            )
                            prev
            )
            def
            bp


toList : Layout -> List ( Cell ( Path, Bounds ) )
toList =
    unfold (::) []
