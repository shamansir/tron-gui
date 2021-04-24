module Tron.Util exposing (..)

import Array exposing (Array)


-- import Bounds exposing (Bounds)


-- type Shape a = Shape ( Int, Int )

-- type Pixels = Pixels

-- type Cells = Cells

-- type BoundsIn a = BoundsIn Bounds


align : Float -> Float
align v =
    if v > 1 then 1.0
    else if v < 0 then 0.0
        else v


findMap : (a -> Maybe x) -> List a -> Maybe x
findMap toValue =
    List.foldl
        (\item maybeResult ->
            case maybeResult of
                Nothing -> toValue item
                _ -> maybeResult
        )
        Nothing


findMapInArray : (a -> Maybe x) -> Array a -> Maybe x
findMapInArray toValue =
    Array.foldl
        (\item maybeResult ->
            case maybeResult of
                Nothing -> toValue item
                _ -> maybeResult
        )
        Nothing


filterMapArray : Array (Maybe a) -> Array a
filterMapArray =
    Array.toList >> List.filterMap identity >> Array.fromList
    -- FIXME: faster with `fold`


alter : { a | min : Float, max : Float, step : Float } -> Float -> Float -> Float
alter { min, max, step } amount curValue =
    let
        toAdd = amount * (max - min)
        alignedByStep = toFloat (floor (toAdd / step)) * step
    in min + alignedByStep
