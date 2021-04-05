module SmartPack exposing (..)


import Array exposing (Array)
import Matrix exposing (Matrix)

import BinPack exposing (Bounds)
import Size exposing (..)
import Html exposing (a)


type Distribution
    = Up
    | Down
    | Right
    | Left
    | UpAndDown
    | LeftAndRight


type SmartPack a = SmartPack (Size Cells) (List (Bounds, a))


map : (a -> b) -> SmartPack a -> SmartPack b
map f (SmartPack size list) =
    SmartPack size <| List.map (Tuple.mapSecond f) <| list


withBounds : (Bounds -> Bounds) -> SmartPack a -> SmartPack a
withBounds f (SmartPack size list) =
    SmartPack size <| List.map (Tuple.mapFirst f) <| list


container : Size Cells -> SmartPack a
container size = SmartPack size []


pack : Distribution -> Size Cells -> a -> SmartPack a -> Maybe (SmartPack a)
pack distribution size v s =
    s
        |> findSpot distribution size
        |> Maybe.map
            (\(x, y) ->
                case ( s, size ) of
                    ( SmartPack ps xs, Size ( w, h ) ) ->
                        SmartPack ps
                            <| ( { x = toFloat x, y = toFloat y, width = toFloat w, height = toFloat h }, v ) :: xs

            )


packAt : ( Int, Int ) -> SizeF Cells -> a -> SmartPack a -> Maybe (SmartPack a)
packAt _ _ _ s = Just s


packCloseTo
    :  Distribution
    -> ( Int, Int )
    -> SizeF Cells
    -> a
    -> SmartPack a
    -> Maybe (SmartPack a)
packCloseTo _ _ _ _ s = Just s


carelessPack : Distribution -> Size Cells -> a -> SmartPack a -> SmartPack a
carelessPack _ _ _ s = s


carelessPackAt : ( Int, Int ) -> Size Cells -> a -> SmartPack a -> SmartPack a
carelessPackAt _ _ _ s = s


carelessPackCloseTo
    :  ( Float, Float )
    -> SizeF Cells
    -> Distribution
    -> a
    -> SmartPack a
    -> SmartPack a
carelessPackCloseTo _ _ _ _ s = s


resize : Size Cells -> SmartPack a -> SmartPack a
resize newSize (SmartPack _ items) =
    SmartPack newSize items


toList : SmartPack a -> List (Bounds, a)
toList (SmartPack _ items) = items


dimensions : SmartPack a -> Size Cells
dimensions (SmartPack size _) = size


toMatrix : SmartPack a -> Matrix ( Maybe a )
toMatrix (SmartPack (Size (w, h)) items) =
    let
        addBounds bounds matrix =
            matrix
    in items
        |> List.sortBy (Tuple.first >> .x)
        |> List.foldl
            (\(bounds, v) ->
                addBounds bounds
            )
            (Matrix.initialize (w, h) <| always Nothing)


findSpot : Distribution -> Size Cells -> SmartPack a -> Maybe ( Int, Int )
findSpot distribution size = toMatrix >> findSpotM distribution size


findSpotM : Distribution -> Size Cells -> Matrix (Maybe a) -> Maybe ( Int, Int )
findSpotM distribution (Size (wc, hc)) matrix =
    let

        isEmpty cell =
            case cell of
                Just _ -> False
                Nothing -> True

        fitsAt (x, y) =
            matrix
                |> Matrix.slice (x, y) (x + wc, y + hc)
                |> foldM (Tuple.second >> isEmpty >> (&&)) True

        firstPos =
            (0, 0)

        maybeNext (x, y) =
            Nothing

        helper (x, y) =
            if fitsAt (x, y) then
                Just (x, y)
            else
                maybeNext (x, y)
                    |> Maybe.andThen helper

    in
        if (wc > 0) && (hc > 0)
            && not (Matrix.isEmpty matrix)
            then
                case Matrix.size matrix of
                    ( mw, mh ) ->
                        if (mw > wc) && (mh > hc)
                            then helper firstPos
                            else Nothing
        else Nothing


find : ( Float, Float ) -> SmartPack a -> Maybe a
find pos = toMatrix >> findM pos


findM : ( Float, Float ) -> Matrix ( Maybe a ) -> Maybe a
findM pos _ = Nothing


fold : (((Int, Int), Maybe a) -> b -> b) -> b -> SmartPack a -> b
fold f init =
    toMatrix >> foldM f init


foldM : (((Int, Int), Maybe a) -> b -> b) -> b -> Matrix (Maybe a) -> b
foldM f init =
    Matrix.toIndexedList
        >> List.foldl f init


get : (Int, Int) -> SmartPack a -> Maybe a
get pos =
    toMatrix
        >> Matrix.get pos
        >> Maybe.andThen identity
