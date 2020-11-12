module Gui.Layout exposing (Layout, Cell(..), init, pack, pack1, unfold, find, toList)


import Array exposing (..)
import Json.Decode as Json


import Bounds exposing (Bounds)
import Gui.Control exposing (Control(..))
import Gui.Path as Path exposing (Path)
import Gui.Property exposing (..)
import Gui.Property as Property exposing (fold)
import BinPack exposing (..)
import Gui.Render.Style exposing (Flow(..), CellShape(..))


type Cell_ a
    = One_ a
    | Many_ a (BinPack a)


type Cell a
    = One a
    | Many a (List a)


type alias Layout = ( Flow, ( Float, Float ), BinPack (Cell_ Path) )


{-
type Cells = Cells

type Pixels = Pixels


type Size a = Size ( Int, Int )


type Position a = Position { x : Float, y : Float }
-}


init : Flow -> ( Float, Float ) -> Layout
init flow size =
    ( flow
    , size
    , initBinPack <| adaptSizeToFlow flow size
    )


initBinPack : ( Float, Float ) -> BinPack a
initBinPack ( maxCellsByX, maxCellsByY )
    = container maxCellsByX maxCellsByY


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


pack : Flow -> ( Float, Float ) -> Property msg -> Layout
pack flow size = pack1 flow size Path.start


pack1 : Flow -> ( Float, Float ) -> Path -> Property msg -> Layout
pack1 flow size rootPath prop =
    case prop of
        Nil ->
            init flow size
        Group (Control ( ( shape, _ ), items) _ _) ->
            ( flow
            , size
            , packItemsAtRoot (adaptSizeToFlow flow size) rootPath shape items
            )
        Choice (Control ( (shape, _ ), items) _ _) ->
            ( flow
            , size
            , packItemsAtRoot (adaptSizeToFlow flow size) rootPath shape items
            )
        _ ->
            ( flow
            , size
            , initBinPack (adaptSizeToFlow flow size)
                |> BinPack.pack1 ( { width = 1, height = 1 }, One_ <| Path.start )
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
            BinPack.pack1
                ( { width = 1, height = 1 }
                , One_ <| Path.fromList path
                )

        packOneSub path cellShape =
            BinPack.pack1
                ( case cellSizeFromShape cellShape of
                    ( cw, ch ) ->
                        { width = cw, height = ch }
                , Path.fromList path
                )

        packMany path (w, h) cellShape plateItems =
            BinPack.pack1
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
            -> BinPack (Cell_ Path)
            -> Control
                    ( ( Shape, CellShape ), Array ( Label, Property msg ) )
                    ( GroupState, a )
                    msg
            -> BinPack (Cell_ Path)
        packGroupControl
            path
            layout
            (Control ( ( innerShape, cellShape ), innerItems ) (grpState, _) _) =
            case grpState of
                Expanded ->
                    let
                        withPlate
                            = layout
                                |> packMany
                                    path
                                    innerShape
                                    cellShape
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
    let
        adaptBounds =
            adaptBoundsToFlow flow <| adaptSizeToFlow flow size
    in
        BinPack.unfold
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


adaptBoundsToFlow : Flow -> ( Float, Float ) -> Bounds -> Bounds
adaptBoundsToFlow flow ( width, height ) innerBounds =
    case flow of
        TopToBottom -> innerBounds
        BottomToTop ->
            { innerBounds
            | y = height - innerBounds.y - innerBounds.height
            }
        LeftToRight ->
            { width = innerBounds.height
            , height = innerBounds.width
            , x = innerBounds.y
            , y = innerBounds.x
            }
        RightToLeft ->
            { width = innerBounds.height
            , height = innerBounds.width
            , x = height - innerBounds.y - innerBounds.height
            , y = innerBounds.x
            }


adaptPosToFlow : Flow -> ( Float, Float ) -> { x : Float, y : Float } -> { x : Float, y : Float }
adaptPosToFlow flow ( width, height ) { x, y } =
    case flow of
        TopToBottom -> { x = x, y = y }
        BottomToTop ->
            { x = x
            , y = height - y
            }
        LeftToRight ->
            { x = y
            , y = x
            }
        RightToLeft ->
            { x = y
            , y = width - x
            }


adaptSizeToFlow : Flow -> ( Float, Float ) -> ( Float, Float )
adaptSizeToFlow flow ( w, h ) =
    case flow of
        TopToBottom -> ( w, h )
        BottomToTop -> ( w, h )
        LeftToRight -> ( h, w )
        RightToLeft -> ( h, w )


cellSizeFromShape : CellShape -> ( Float, Float )
cellSizeFromShape cs =
    case cs of
       Full -> ( 1.0, 1.0 )
       Half -> ( 0.5, 0.5 )
       HalfByOne -> ( 0.5, 1.0 )
       OneByHalf -> ( 1.0, 0.5 )
       TwiceByHalf -> ( 2.0, 0.5 )
       HalfByTwice -> ( 0.5, 2.0 )
       TwiceByTwice -> ( 2.0, 2.0 )

