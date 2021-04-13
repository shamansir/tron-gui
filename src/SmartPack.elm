module SmartPack exposing
    ( Distribution(..)
    , Position
    , SmartPack
    , map, withBounds
    , container
    , pack, packAt, packCloseTo
    , resize, dimensions
    , toMatrix, toList
    , fold, get, find
    )


import Bounds exposing (Bounds)


import Array exposing (Array)
import Matrix exposing (Matrix)

import Size exposing (..)

type alias Position = (Int, Int)


type Distribution
    = Up
    | Down
    | Right
    | Left


type SmartPack a = SmartPack (Size Cells) (List (Bounds, a))


map : (a -> b) -> SmartPack a -> SmartPack b
map f (SmartPack size list) =
    SmartPack size <| List.map (Tuple.mapSecond f) <| list


withBounds : (Bounds -> Bounds) -> SmartPack a -> SmartPack a
withBounds f (SmartPack size list) =
    SmartPack size <| List.map (Tuple.mapFirst f) <| list


container : Size Cells -> SmartPack a
container size = SmartPack size []


pack : Distribution -> Size Cells -> a -> SmartPack a -> Maybe (Position, SmartPack a)
pack distribution size v sp =
    sp
        |> findSpot distribution size
        |> Maybe.map
            (\pos ->
                ( pos, sp |> forceAdd pos size v )
            )


packAt : Position -> Size Cells -> a -> SmartPack a -> Maybe (SmartPack a)
packAt pos size v sp =
    if sp |> fitsAt pos size then
        Just <| forceAdd pos size v <| sp
    else Nothing


packCloseTo
    :  Distribution
    -> Position
    -> Size Cells
    -> a
    -> SmartPack a
    -> Maybe (Position, SmartPack a)
packCloseTo distribution pos size v sp =
    sp
        |> findSpotCloseTo distribution pos size
        |> Maybe.map
            (\foundPos ->
                ( foundPos, sp |> forceAdd foundPos size v )
            )


forceAdd : Position -> Size Cells -> a -> SmartPack a -> SmartPack a
forceAdd (x, y) (Size ( w, h )) v (SmartPack ps xs) =
    SmartPack ps <|
        (
            { x = x
            , y = y
            , width = w
            , height = h
            }
        , v )
        :: xs


resize : Size Cells -> SmartPack a -> SmartPack a
resize newSize (SmartPack _ items) =
    SmartPack newSize items


toList : SmartPack a -> List (Bounds, a)
toList (SmartPack _ items) = items


dimensions : SmartPack a -> Size Cells
dimensions (SmartPack size _) = size


toMatrix : SmartPack a -> Matrix ( Maybe a )
toMatrix =
    toMatrixWithBounds >> Matrix.map (Maybe.map Tuple.second)

toMatrixWithBounds : SmartPack a -> Matrix ( Maybe ( Bounds, a ) )
toMatrixWithBounds (SmartPack (Size (w, h)) items) =
    let
        fill bounds v matrix =
            List.range bounds.x (bounds.x + bounds.width - 1)
                |> List.map
                    (\x ->
                        List.range bounds.y (bounds.y + bounds.height - 1)
                            |> List.map (Tuple.pair x)
                    )
                |> List.concat
                |> List.foldl (\pos -> Matrix.set pos <| Just ( bounds, v )) matrix
    in items
        |> List.sortBy (Tuple.first >> .x)
        |> List.foldl
            (\(bounds, v) ->
                fill bounds v
            )
            (Matrix.initialize (w, h) <| always Nothing)


isEmpty : Maybe a -> Bool
isEmpty cell =
    case cell of
        Just _ -> False
        Nothing -> True


fitsAt : Position -> Size Cells -> SmartPack a -> Bool
fitsAt pos size =
    toMatrix >> fitsAtM pos size


fitsAtM :Position -> Size Cells -> Matrix (Maybe a) -> Bool
fitsAtM (x, y) (Size (cw, ch)) matrix =
    let (mw, mh) = Matrix.size matrix
    in
    if x + cw <= mw && y + ch <= mh then
        matrix
            |> Matrix.slice (x, y) (x + cw, y + ch)
            |> foldM (Tuple.second >> isEmpty >> (&&)) True
    else False


findSpot : Distribution -> Size Cells -> SmartPack a -> Maybe Position
findSpot distribution size = toMatrix >> findSpotM distribution size


findSpotM : Distribution -> Size Cells -> Matrix (Maybe a) -> Maybe Position
findSpotM distribution (Size (cw, ch) as size) matrix =
    let
        ( mw, mh ) = Matrix.size matrix
        fitsAt_ (x, y) =
            fitsAtM (x, y) size matrix

        firstPos d =
            case d of
                Down -> (0, 0)
                Right -> (0, 0)
                Left -> (mw - 1, 0)
                Up -> (0, mh - 1)

        maybeNext d (x, y) =
            case d of
                Down ->
                    if y < mh - 1 then
                        Just (x, y + 1)
                    else if x < mw - 1 then
                        Just (x + 1, 0)
                    else Nothing
                Up ->
                    if y > 0 then
                        Just (x, y - 1)
                    else if x < mw - 1 then
                        Just (x + 1, mh - 1)
                    else Nothing
                Right ->
                    if x < mw - 1 then
                        Just (x + 1, y)
                    else if y < mh - 1 then
                        Just (0, y + 1)
                    else Nothing
                Left ->
                    if x > 0 then
                        Just (x - 1, y)
                    else if y < mh - 1 then
                        Just (mw - 1, y + 1)
                    else Nothing

        helper d (x, y) =
            if fitsAt_ (x, y) then
                Just (x, y)
            else
                maybeNext d (x, y) |> Maybe.andThen (helper d)

    in
        if (cw > 0) && (ch > 0)
            && not (Matrix.isEmpty matrix)
            && (mw >= cw) && (mh >= ch)
            then helper distribution <| firstPos distribution
            else Nothing


findSpotCloseTo : Distribution -> Position -> Size Cells -> SmartPack a -> Maybe Position
findSpotCloseTo distribution pos size = toMatrix >> findSpotCloseToM distribution pos size


findSpotCloseToM : Distribution -> Position -> Size Cells -> Matrix (Maybe a) -> Maybe Position
findSpotCloseToM distribution (px, py) (Size (cw, ch) as size) matrix =
    let
        ( mw, mh ) = Matrix.size matrix
        fitsAt_ (x, y) =
            fitsAtM (x, y) size matrix

        maybeNext d (x, y) =
            case d of
                Down ->
                    if y < mh - 1 then
                        Just (x, y + 1)
                    else if x < mw - 1 then
                        Just (x + 1, py)
                    else Nothing
                Up ->
                    if y > 0 then
                        Just (x, y - 1)
                    else if x < mw - 1 then
                        Just (x + 1, py)
                    else Nothing
                Right ->
                    if x < mw - 1 then
                        Just (x + 1, y)
                    else if y < mh - 1 then
                        Just (px, y + 1)
                    else Nothing
                Left ->
                    if x > 0 then
                        Just (x - 1, y)
                    else if y < mh - 1 then
                        Just (px, y + 1)
                    else Nothing

        helper d (x, y) =
            if fitsAt_ (x, y) then
                Just (x, y)
            else
                maybeNext d (x, y) |> Maybe.andThen (helper d)

    in
        if (cw > 0) && (ch > 0)
            && not (Matrix.isEmpty matrix)
            && (mw >= cw) && (mh >= ch)
            then helper distribution (px, py)
            else Nothing


find : ( Float, Float ) -> SmartPack a -> Maybe ( Bounds, a )
-- find pos = always Nothing
find pos = toMatrixWithBounds >> findM pos
   -- FIXME: use the stored bounds instead of Matrix? could be faster!


findM : ( Float, Float ) -> Matrix ( Maybe ( Bounds, a ) ) -> Maybe ( Bounds, a )
findM ( x, y ) =
    Matrix.get ( floor x, floor y ) >> Maybe.andThen identity


fold : ((Position, Maybe a) -> b -> b) -> b -> SmartPack a -> b
fold f init =
    toMatrix >> foldM f init


foldM : ((Position, Maybe a) -> b -> b) -> b -> Matrix (Maybe a) -> b
foldM f init =
    Matrix.toIndexedList
        >> List.foldl f init


get : Position -> SmartPack a -> Maybe a
get pos =
    toMatrix
        >> Matrix.get pos
        >> Maybe.andThen identity
