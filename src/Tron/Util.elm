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


alignByRange { min, max } v =
    if v > max then max
    else if v < min then min
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


alter : { min : Float, max : Float, step : Float } -> Float -> Float -> Float
alter axis amount curValue =
    let
        range_ = axis.max - axis.min
        -- _ =  Debug.log "curValue" curValue
        -- _ =  Debug.log "amount" amount

        adjusted = curValue + ( amount * range_ / 20 ) --|> Debug.log "adjusted"
        -- alignedByStep = adjusted
        alignedByStep =
            adjusted
            {-
            if (adjusted < 0) then
                (toFloat (floor (adjusted / axis.step)) * axis.step)
            else
                (toFloat (ceiling (adjusted / axis.step)) * axis.step)
            -}
    in alignByRange axis <| alignedByStep

