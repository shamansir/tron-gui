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


{-
    `SmartPack` stores all bounds and positions as `Int` due to its Matrix-based nature.
    But `Cells` can be half-sized. So we divide and multiply sizes by 2 when going back and forth.
    The rule is that `Layout` operates and exposes API with usual `Float`-based cells where `Unit` is `One` cell. While inside, when working with `SmartPack`, it proportionally changes the sizes to their `Int` analogues.
-}
type alias Layout = ( SizeF Cells, SmartPack (Cell_ Path) )


--type Position a = Position { x : Float, y : Float }


init : SizeF Cells -> Layout
init size =
    ( size
    , SmartPack.container <| adaptSize size
    )


adaptSize : SizeF Cells -> Size Cells
adaptSize (SizeF ( w, h )) =
    Size ( ceiling <| w * 2, ceiling <| h * 2 )


find : Layout -> { x : Float, y : Float } -> Maybe Path
find ( size, layout ) { x, y } =
    case layout |> SmartPack.find ( x * 2, y * 2 ) of
        Just ( _, One_ path ) ->
            Just path
        Just ( bounds, Many_ _ innerPages ) ->
            innerPages
                |> Pages.getCurrent
                |> Maybe.andThen
                    (SmartPack.find
                        ( x * 2 - Basics.toFloat bounds.x
                        , y * 2 - Basics.toFloat bounds.y
                        )
                    )
                |> Maybe.map Tuple.second
        Nothing ->
            Nothing


pack : SizeF Cells -> Property msg -> Layout
pack size = pack1 size Path.start


pack1 : SizeF Cells -> Path -> Property msg -> Layout
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
            , SmartPack.container (adaptSize size)
                |> SmartPack.carelessPack
                    D.Right
                    ( Size ( 2, 2 ) )
                    ( One_ <| Path.start )
            )


packItemsAtRoot
    :  SizeF Cells
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
                    (SmartPack.container <| adaptSize size)

        packOne path =
            SmartPack.carelessPack
                D.Right
                ( Size ( 2, 2 ) )
                <| One_ <| Path.fromList path

        packOneSub path cellShape =
            let
                cellShape_ =
                    Size
                        <| case CS.numify cellShape of
                            ( cw, ch ) ->
                                ( ceiling <| cw * 2
                                , ceiling <| ch * 2
                                )
            in SmartPack.carelessPack
                D.Right
                cellShape_
                <| Path.fromList path

        packMany path pageNum panelShape cellShape plateItems =
            let
                ( pageCount, SizeF ( pageWidthF, pageHeightF ) ) =
                    Property.findShape panelShape cellShape <| Array.toList plateItems
                pageSize =
                    Size
                        ( ceiling <| pageWidthF * 2
                        , ceiling <| pageHeightF * 2
                        )

            in

                SmartPack.carelessPack
                    D.Right
                    pageSize
                    (Many_
                            (Path.fromList path)
                            <| Pages.map
                                (List.foldl
                                    (\(index, innerProp) plateLayout ->
                                        if not <| isGhost innerProp
                                            then packOneSub (path ++ [index]) cellShape plateLayout
                                            else plateLayout
                                    )
                                    ( SmartPack.container pageSize )
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


fold : ( Cell ( BoundsF, Path ) -> a -> a ) -> a -> Layout -> a
fold f def ( _, sp ) =
    let
        cv bounds =
            bounds |> Bounds.toFloat |> Bounds.divideBy 2
    in sp
        |> SmartPack.toList
        |> List.foldl
            (\( bounds, c ) prev ->
                case c of
                    One_ path ->
                        f
                            ( One ( cv bounds, path ) )
                            prev

                    Many_ path bpPages ->
                        f
                            ( Many
                                ( cv bounds, path )
                                <| Pages.map
                                    (List.map
                                        (Tuple.mapFirst
                                            <| cv << Bounds.shift bounds
                                        )
                                    << SmartPack.toList
                                    )
                                <| bpPages
                            )
                            prev
            )
            def


toList : Layout -> List ( Cell ( BoundsF, Path ) )
toList =
    fold (::) []
