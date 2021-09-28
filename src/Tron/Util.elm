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


alter : { a | min : Float, max : Float, step : Float } -> Float -> Float -> Float
alter { min, max, step } amount startedFrom =
    let
        toAdd = (amount - 0.5) * (max - min)
        adjustedValue = min + (toFloat (floor ((startedFrom - min + toAdd) / step)) * step)
    in if adjustedValue <= min then min
        else if adjustedValue >= max then max
        else adjustedValue
