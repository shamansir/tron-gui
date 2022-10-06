module Tron.Layout exposing (Layout, Cell(..), init, pack, pack1, fold, find, toList, size)


import Array exposing (..)
import Json.Decode as Json
import Dict


import Bounds exposing (..)
import SmartPack exposing (..)
import SmartPack as D exposing (Distribution(..))
import Size exposing (..)

import Tron.Control exposing (Control(..))
import Tron.Path as Path exposing (Path)
import Tron.Tree.Internals exposing (..)
import Tron.Tree.Internals as Tree exposing (fold)
import Tron.Tree.Controls exposing (isGhost)
import Tron.Style.Dock exposing (Dock)
import Tron.Style.Dock as Dock
import Tron.Style.Logic as Dock exposing (rootPosition)
import Tron.Style.CellShape exposing (CellShape)
import Tron.Style.CellShape as CS
import Tron.Style.PanelShape as PS exposing (PanelShape)
import Tron.Pages as Pages exposing (Pages, PageNum)

import Tron.Control.Impl.Nest exposing (Form(..))
import Tron.Control.Impl.Nest as Nest exposing (getItems)



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
-- FIXME : change size to Int size, it cannot be fractional even when cells can be


--type Position a = Position { x : Float, y : Float }


init : SizeF Cells -> Layout
init size_ =
    ( size_
    , SmartPack.container <| adaptSize size_
    )


adaptSize : SizeF Cells -> Size Cells
adaptSize (SizeF ( w, h )) =
    Size ( ceiling <| w * 2, ceiling <| h * 2 )


size : Layout -> SizeF Cells
size = Tuple.first


find : Layout -> { x : Float, y : Float } -> Maybe Path
find ( _, layout ) { x, y } =
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


pack : Dock -> SizeF Cells -> Tree a -> Layout
pack dock size_ = pack1 dock size_ Path.start


pack1 : Dock -> SizeF Cells -> Path -> Tree a -> Layout
pack1 dock size_ rootPath prop =
    case prop of
        Nil _ ->
            init size_
        Group _ ( shape, _ ) control ->
            ( size_
            , packItemsAtRoot dock size_ rootPath shape
                <| Nest.getItems control
            )
        Choice _ ( shape, _ ) control ->
            ( size_
            , packItemsAtRoot dock size_ rootPath shape
                <| Nest.getItems control
            )
        _ ->
            let
                emptyContainer = SmartPack.container (adaptSize size_)
            in
                ( size_
                , emptyContainer
                    |> SmartPack.pack
                        D.Right
                        ( Size ( 2, 2 ) )
                        ( One_ <| Path.start )
                    |> Maybe.map Tuple.second
                    |> Maybe.withDefault emptyContainer
                )


packItemsAtRoot
    :  Dock
    -> SizeF Cells
    -> Path
    -> PanelShape
    -> Array (Path.Label, Tree a)
    -> SmartPack (Cell_ Path)
packItemsAtRoot dock size_ rp shape items =
    let
        rootPath = Path.toList rp
        orLayout l =
            Maybe.map Tuple.second
                >> Maybe.withDefault l
        visibleCount =
            Array.length items
                - (items |> Array.filter (Tuple.second >> isGhost) |> Array.length)

        itemsAndPositions =
            items
                |> Array.foldl
                    (\(label, item) ( visIndex, prevItems ) ->
                        if not <| isGhost item then
                            ( visIndex + 1
                            ,
                                ( Dock.rootPosition
                                    dock
                                    (Size.toInt size_)
                                    visibleCount
                                    <| visIndex
                                , ( label, item )
                                ) :: prevItems
                            )
                        else ( visIndex, ( (-1, -1), ( label, item ) ) :: prevItems )
                    )
                    ( 0, [] )
                |> Tuple.second
                |> List.reverse
                |> Array.fromList

        firstLevelLayout : SmartPack (Cell_ Path)
        firstLevelLayout =
            itemsAndPositions
                |> Array.indexedMap Tuple.pair
                |> Array.foldl
                    (\(index, ( pos, ( label, theProp ) ) ) layout ->
                        if not <| isGhost theProp
                            then
                                layout
                                    |> packOneAt (rootPath ++ [ ( index, label )]) pos
                                    |> Maybe.withDefault layout
                            else layout
                    )
                    ( SmartPack.container <| adaptSize size_ )

        packOneAt path ( x, y ) =
            SmartPack.packAt
                ( x * 2, y * 2 )
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
            in SmartPack.pack
                D.Right
                cellShape_
                <| Path.fromList path

        -- packMany
        --      : List (Path.Index, Path.Label)
        --     -> Int
        --     -> PanelShape
        --     -> CellShape
        --     -> Array (Path.Label, Tree a)
        --     -> (Int, Int)
        --     -> SmartPack (Cell_ Path)
        --     -> SmartPack (Cell_ Path)
        packMany path pageNum panelShape cellShape plateItems parentPos layout =
            let
                parentIdx =
                    path
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ( -1, "" )
                ( itemsOverPages, SizeF ( pageWidthF, pageHeightF ) ) =
                    PS.distribute panelShape cellShape Pages.First -- FIXME: use PageRef
                        <| Array.toList
                        <| Array.filter (Tuple.second >> Tuple.second >> isGhost >> not)
                        <| Array.indexedMap Tuple.pair
                        <| plateItems
                pageSize =
                    Size
                        ( ceiling <| pageWidthF * 2
                        , ceiling <| pageHeightF * 2
                        )
                packedPages =
                    itemsOverPages
                        |> Pages.switchTo pageNum
                        |> Pages.map
                            (List.foldl
                                (\(index, ( label, innerProp)) ( plateLayout, positions ) ->
                                    if not <| isGhost innerProp
                                        then
                                            case packOneSub
                                                (path ++ [(index, label)])
                                                cellShape
                                                plateLayout of
                                                Just ( pos, nextLayout_ ) ->
                                                    ( nextLayout_
                                                    , positions
                                                        |> Dict.insert (path ++ [(index, label)]) pos
                                                    )
                                                Nothing ->
                                                    ( plateLayout, positions )
                                        else ( plateLayout, positions )
                                )
                                ( SmartPack.container pageSize, Dict.empty )
                            )
                many =
                    Many_
                        (Path.fromList path)
                        (packedPages |> Pages.map Tuple.first)


                maybePackedCloseTo =
                    {- SmartPack.pack
                        D.Right -}
                    SmartPack.packCloseTo
                        (Dock.toDistribution dock <| Tuple.first parentIdx)
                        (case parentPos of
                            (x, y) -> (x * 2, y * 2)
                        )
                        pageSize
                        many
                        layout
                nextLayout =
                    case maybePackedCloseTo of
                        Just ( _, layoutWithPlate ) -> layoutWithPlate
                        Nothing ->
                            SmartPack.pack
                                (Dock.toDistribution dock <| Tuple.first parentIdx)
                                pageSize
                                many
                                layout
                                |> orLayout layout
            in

                ( nextLayout
                , packedPages
                    |> Pages.map Tuple.second
                    |> Pages.fold Dict.union Dict.empty
                )

        packGroupControl
            :  List ( Path.Index, Path.Label )
            -> ( PanelShape, CellShape )
            -> Position
            -> SmartPack (Cell_ Path)
            -> Control
                    ( Array ( Path.Label, Tree a ) )
                    { r | form : Form, page : PageNum }
                    a
            -> SmartPack (Cell_ Path)
        packGroupControl
            path
            ( panelShape, cellShape )
            parentPos
            layout
            control =
            if Nest.is Expanded control then
                let
                    items_ =
                        Nest.getItems control

                    ( layoutWithPlate, positions )
                        = layout
                            |> packMany
                                path
                                (Nest.getPage control)
                                panelShape
                                cellShape
                                items_
                                parentPos
                    itemsAndPositions_ : Array ( D.Position, ( Path.Label, Tree.Tree a ) )
                    itemsAndPositions_ =
                        items_ |>
                            Array.indexedMap
                                (\index (label, item) ->
                                    ( positions
                                        |> Dict.get (path ++ [(index, label)])
                                        |> Maybe.map
                                            (\(x, y) ->
                                                case parentPos of
                                                    ( px, py ) -> ( px + x, py + y)
                                            )
                                        |> Maybe.withDefault (0, 0)
                                    , ( label, item )
                                    )
                                )
                in
                    packPlatesOf path layoutWithPlate itemsAndPositions_
            else layout

        --packPlatesOf : Path -> Array ( D.Position, ( Path.Label, Tree.Tree a ) ) -> SmartPack (Cell_ Path)
        packPlatesOf path layout =
            Array.indexedMap Tuple.pair -- FIXME: consider pages
                >> Array.foldl
                    (\(index, ( pos, ( label, innerProp ) ) ) prevLayout ->
                        let
                            nextPath = path ++ [(index, label)]
                        in
                        case innerProp of
                            Choice _ innerShape control ->
                                control |> packGroupControl nextPath innerShape pos prevLayout
                            Group _ innerShape control ->
                                control |> packGroupControl nextPath innerShape pos prevLayout
                            _ ->
                                prevLayout
                    )
                    layout

    in
        itemsAndPositions |> packPlatesOf rootPath firstLevelLayout


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
