module Gui.Layout exposing (Layout, Cell(..), init, pack, pack1, unfold, find, toList)


import Array exposing (..)
import Json.Decode as Json


import Bounds exposing (Bounds)
import Gui.Control exposing (Control(..))
import Gui.Path as Path exposing (Path)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (fold)
import BinPack exposing (..)
import Gui.Render.Style exposing (Flow(..))


type Cell_ a
    = One_ a
    | Many_ a (BinPack a)


type Cell a
    = One a
    | Many a (List a)


type alias Layout = ( Flow, ( Int, Int ), BinPack (Cell_ Path) )


{-
type Cells = Cells

type Pixels = Pixels


type Size a = Size ( Int, Int )


type Position a = Position { x : Float, y : Float }
-}


init : Flow -> ( Int, Int ) -> Layout
init flow size
    = ( flow, size, initBinPack size )


initBinPack : ( Int, Int ) -> BinPack a
initBinPack ( maxCellsByX, maxCellsByY )
    = container (toFloat maxCellsByX) (toFloat maxCellsByY)


find : Layout -> { x : Float, y : Float } -> Maybe Path
find ( flow, size, layout ) pos =
    let
        adaptedPos = adaptPosToFlow flow size pos
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


pack : Flow -> ( Int, Int ) -> Property msg -> Layout
pack flow size = pack1 flow size Path.start


pack1 : Flow -> ( Int, Int ) -> Path -> Property msg -> Layout
pack1 flow size rootPath prop =
    case prop of
        Nil ->
            init flow size
        Group (Control (shape, items) _ _) ->
            ( flow
            , size
            , packItemsAtRoot size rootPath shape items
            )
        Choice (Control (shape, items) _ _) ->
            ( flow
            , size
            , packItemsAtRoot size rootPath shape items
            )
        _ ->
            ( flow
            , size
            , initBinPack size
                |> BinPack.pack1 ( { width = 1, height = 1 }, One_ <| Path.start )
            )


packItemsAtRoot
    :  ( Int, Int )
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
            BinPack.pack1
                ( { width = 1, height = 1 }
                , One_ <| Path.fromList path
                )

        packOne1 path =
            BinPack.pack1
                ( { width = 1, height = 1 }
                , Path.fromList path
                )

        packMany path (w, h) plateItems =
            BinPack.pack1
                (
                    { width = toFloat w
                    , height = toFloat h
                    }
                , Many_
                    (Path.fromList path)
                    <| Array.foldl
                        (\(index, ( _, innerProp)) plateLayout ->
                            if not <| isGhost innerProp
                                then packOne1 (path ++ [index]) plateLayout
                                else plateLayout
                        )
                        (BinPack.container (toFloat w) (toFloat h))
                    <| Array.indexedMap Tuple.pair
                    <| plateItems
                )

        packGroupControl
            :  List Int
            -> BinPack (Cell_ Path)
            -> Control
                    ( Shape, Array ( Label, Property msg ) )
                    ( GroupState, a )
                    msg
            -> BinPack (Cell_ Path)
        packGroupControl path layout (Control ( innerShape, innerItems ) (grpState, _) _) =
            case grpState of
                Expanded ->
                    let
                        withPlate
                            = layout
                                |> packMany
                                    path
                                    innerShape
                                    innerItems
                    in
                        packPlatesOf path withPlate innerItems
                Collapsed -> layout
                Detached -> layout

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
        items |> packPlatesOf rootPath firstLevel


unfold : ( Cell ( Path, Bounds ) -> a -> a ) -> a -> Layout -> a
unfold f def ( flow, size, bp ) =
    BinPack.unfold
        (\(c, bounds) prev ->
            case c of
                One_ path ->
                    f
                        ( One ( path, adaptToFlow flow size bounds ) )
                        prev

                Many_ path innerBp ->
                    f
                        ( Many
                            ( path, bounds )
                            <| List.map
                                (Tuple.mapSecond
                                    <| adaptToFlow flow size << Bounds.shift bounds
                                )
                            <| BinPack.unfold
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


adaptToFlow : Flow -> ( Int, Int ) -> Bounds -> Bounds
adaptToFlow flow ( width, height ) innerBounds =
    case flow of
        TopToBottom -> innerBounds
        BottomToTop ->
            { innerBounds
            | y = toFloat height - innerBounds.y - innerBounds.height
            }
        _ -> innerBounds


adaptPosToFlow : Flow -> ( Int, Int ) -> { x : Float, y : Float } -> { x : Float, y : Float }
adaptPosToFlow flow ( width, height ) ({ x, y } as pos) =
    case flow of
        TopToBottom -> pos
        BottomToTop ->
            { pos
            | y = toFloat height - y
            }
        _ -> pos
