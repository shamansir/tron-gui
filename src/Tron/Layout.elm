module Tron.Layout exposing (Layout, Cell(..), init, pack, pack1, fold, find, toList)


import Array exposing (..)
import Json.Decode as Json


import Bounds exposing (..)
import SmartPack exposing (..)
import SmartPack as D exposing (Distribution(..))
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
    | Many_ a (Pages (SmartPack a))


type Cell a
    = One a
    | Many a (Pages (List a))


type alias Layout = ( Size Cells, SmartPack (Cell_ Path) )


--type Position a = Position { x : Float, y : Float }


init : Size Cells -> Layout
init size =
    ( size
    , SmartPack.container size
    )


find : Layout -> { x : Float, y : Float } -> Maybe Path
find ( size, layout ) { x, y } =
    case layout |> SmartPack.find ( x, y ) of
        Just ( _, One_ path ) ->
            Just path
        Just ( bounds, Many_ _ innerPages ) ->
            innerPages
                |> Pages.getCurrent
                |> Maybe.andThen
                    (SmartPack.find
                        ( x - bounds.x
                        , y - bounds.y
                        )
                    )
                |> Maybe.map Tuple.second
        Nothing ->
            Nothing


pack : Size Cells -> Property msg -> Layout
pack size = pack1 size Path.start


pack1 : Size Cells -> Path -> Property msg -> Layout
pack1 size rootPath prop =
    case prop of
        Nil ->
            init size
        Group _ ( shape, _ ) control ->
            ( size
            , packItemsAtRoot size rootPath shape
                <| Nest.getItems control
            )
        Choice _ ( shape, _ ) control ->
            ( size
            , packItemsAtRoot size rootPath shape
                <| Nest.getItems control
            )
        _ ->
            ( size
            , SmartPack.container size
                |> SmartPack.carelessPack
                    D.Up
                    ( Size ( 2, 2 ) )
                    ( One_ <| Path.start )
            )


packItemsAtRoot
    :  Size Cells
    -> Path
    -> PanelShape
    -> Array (Label, Property msg)
    -> SmartPack (Cell_ Path)
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
                    (SmartPack.container size)

        packOne path =
            SmartPack.carelessPack
                D.Up
                ( Size ( 2, 2 ) )
                <| One_ <| Path.fromList path

        packOneSub path cellShape =
            SmartPack.carelessPack
                D.Up
                ( Size <| CS.numify cellShape )
                <| Path.fromList path

        packMany path pageNum panelShape cellShape plateItems =
            let
                ( pageCount, SizeF ( pageWidth, pageHeight ) ) =
                    Property.findShape panelShape cellShape <| Array.toList plateItems
            in

                SmartPack.carelessPack
                    D.Up
                    ( Size
                        ( pageWidth
                        , pageHeight
                        )
                    )
                    (Many_
                            (Path.fromList path)
                            <| Pages.map
                                (List.foldl
                                    (\(index, innerProp) plateLayout ->
                                        if not <| isGhost innerProp
                                            then packOneSub (path ++ [index]) cellShape plateLayout
                                            else plateLayout
                                    )
                                    ( SmartPack.container <| Size ( pageWidth, pageHeight) )
                                )
                            <| Pages.switchTo pageNum
                            <| Pages.distribute 9
                            -- <| Pages.distributeOver pageCount
                            <| Array.toList
                            <| Array.indexedMap Tuple.pair
                            <| plateItems
                    )

        packGroupControl
            :  List Int
            -> ( PanelShape, CellShape )
            -> SmartPack (Cell_ Path)
            -> Control
                    ( Array ( Label, Property msg ) )
                    { a | form : Form, page : PageNum }
                    msg
            -> SmartPack (Cell_ Path)
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


fold : ( Cell ( Bounds, Path ) -> a -> a ) -> a -> Layout -> a
fold f def ( _, sp ) =
    sp
        |> SmartPack.toList
        |> List.foldl
            (\( bounds, c ) prev ->
                case c of
                    One_ path ->
                        f
                            ( One ( bounds, path ) )
                            prev

                    Many_ path bpPages ->
                        f
                            ( Many
                                ( bounds, path )
                                <| Pages.map
                                    (List.map
                                        (Tuple.mapFirst
                                            <| Bounds.shift bounds
                                        )
                                    << SmartPack.toList
                                    )
                                <| bpPages
                            )
                            prev
            )
            def



toList : Layout -> List ( Cell ( Bounds, Path ) )
toList =
    fold (::) []
