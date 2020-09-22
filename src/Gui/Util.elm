module Gui.Util exposing (..)


import Gui.Control exposing (..)


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


alter : { a | min : Float, max : Float, step : Float } -> Float -> Float -> Float
alter { min, max, step } amount curValue =
    let
        toAdd = amount * (max - min)
        alignedByStep = toFloat (floor (toAdd / step)) * step
    in min + alignedByStep
