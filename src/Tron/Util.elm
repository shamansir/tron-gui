module Tron.Util exposing (..)

import Array exposing (Array)
import Task


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


filterMapArray : (a -> Maybe b) -> Array a -> Array b
filterMapArray f =
    Array.toList >> List.filterMap f >> Array.fromList
    -- FIXME: faster with `fold`


catMaybesArray : Array (Maybe a) -> Array a
catMaybesArray =
    filterMapArray identity


flipMaybe : (a, Maybe b) -> Maybe (a, b)
flipMaybe ( a, maybeB ) =
    maybeB |> Maybe.map (Tuple.pair a)


alter : { a | min : Float, max : Float, step : Float } -> Float -> Float -> Float
alter { min, max, step } amount startedFrom =
    let
        toAdd = (amount - 0.5) * (max - min)
        adjustedValue = min + toFloat (floor (startedFrom + toAdd - min / step)) * step
    in if adjustedValue <= min then min
        else if adjustedValue >= max then max
        else adjustedValue


runMaybe : Maybe msg -> Cmd msg
runMaybe maybeMsg =
    case maybeMsg of
        Just msg ->
            Task.succeed msg
                |> Task.perform identity
        Nothing -> Cmd.none


zipArrays : Array a -> Array b -> Array ( Maybe a, Maybe b )
zipArrays arrayA arrayB =
    if Array.length arrayA >= Array.length arrayB then
        arrayA |>
            Array.indexedMap
                (\index valueA ->
                    ( Just valueA, Array.get index arrayB )
                )
    else
        arrayB |>
            Array.indexedMap
                (\index valueB ->
                    ( Array.get index arrayA, Just valueB )
                )


zip3Arrays : Array a -> Array b -> Array c -> Array ( Maybe a, Maybe b, Maybe c )
zip3Arrays arrayA arrayB arrayC =
    if Array.length arrayC < Array.length arrayA || Array.length arrayC < Array.length arrayB then
        zipArrays arrayA arrayB
            |> Array.indexedMap
                (\index ( maybeValueA, maybeValueB ) ->
                    ( maybeValueA, maybeValueB, Array.get index arrayC )
                )
    else if Array.length arrayA >= Array.length arrayB then
        zipArrays arrayA arrayC
            |> Array.indexedMap
                (\index ( maybeValueA, maybeValueC ) ->
                    ( maybeValueA, Array.get index arrayB, maybeValueC )
                )
    else
        zipArrays arrayB arrayC
            |> Array.indexedMap
                (\index ( maybeValueB, maybeValueC ) ->
                    ( Array.get index arrayA, maybeValueB, maybeValueC )
                )
