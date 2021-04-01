module SmartPack exposing (..)


import Array exposing (Array)
import Matrix exposing (Matrix)

import BinPack exposing (Bounds)
import Size exposing (..)


type Distribution
    = Up
    | Down
    | Right
    | Left
    | UpAndDown
    | LeftAndRight


type SmartPack a = SmartPack (Size Cells) (List (Bounds, a))


container : Size Cells -> SmartPack a
container size = SmartPack size []


pack : SizeF Cells -> a -> SmartPack a -> Maybe (SmartPack a)
pack size v s =
    s
        |> findSpot size
        |> Maybe.map
            (\(x, y) ->
                case ( s, size ) of
                    ( SmartPack ps xs, SizeF ( w, h ) ) ->
                        SmartPack ps
                            <| ( { x = x, y = y, width = w, height = h }, v ) :: xs

            )


packAt : ( Float, Float ) -> SizeF Cells -> a -> SmartPack a -> Maybe (SmartPack a)
packAt _ _ _ s = Just s


packCloseTo
    :  ( Float, Float )
    -> SizeF Cells
    -> Distribution
    -> a
    -> SmartPack a
    -> Maybe (SmartPack a)
packCloseTo _ _ _ _ s = Just s


carelessPack : SizeF Cells -> a -> SmartPack a -> SmartPack a
carelessPack _ _ s = s


carelessPackAt : ( Float, Float ) -> SizeF Cells -> a -> SmartPack a -> SmartPack a
carelessPackAt _ _ _ s = s


carelessPackCloseTo
    :  ( Float, Float )
    -> SizeF Cells
    -> Distribution
    -> a
    -> SmartPack a
    -> SmartPack a
carelessPackCloseTo _ _ _ _ s = s


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


findSpot : SizeF Cells -> SmartPack a -> Maybe ( Float, Float )
findSpot size = toMatrix >> findSpotM size


findSpotM : SizeF Cells -> Matrix (Maybe a) -> Maybe ( Float, Float )
findSpotM _ _ = Just ( 0, 0 )


find : ( Float, Float ) -> SmartPack a -> Maybe a
find pos = toMatrix >> findM pos


findM : ( Float, Float ) -> Matrix ( Maybe a ) -> Maybe a
findM pos _ = Nothing
